package it.unibo.agar.controller

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import it.unibo.agar.model.EatingManager
import it.unibo.agar.model.Food
import it.unibo.agar.model.Player
import it.unibo.agar.model.World
import it.unibo.agar.model.AIMovement
import akka.actor.ActorSystem
import akka.actor.CoordinatedShutdown
import akka.cluster.Cluster

import scala.concurrent.ExecutionContextExecutor
import scala.util.Success
import scala.util.Failure
import scala.util.Random
import scala.concurrent.duration.*

object GameStateManager:

  sealed trait Command

  // Commands for player management
  case class MovePlayer(id: String, dx: Double, dy: Double) extends Command
  case class GetWorld(replyTo: ActorRef[World]) extends Command
  private case object InternalTick extends Command

  // Messages for inter-manager communication
  case class RegisterManager(managerId: String, manager: ActorRef[Command]) extends Command
  case class PlayerJoined(playerId: String, player: Player) extends Command
  case class SyncWorldState(world: World) extends Command
  case class PlayerLeft(playerId: String) extends Command

  private case class PlayerMovement(playerId: String, x: Double, y: Double, mass: Double) extends Command
  private case class PlayerEaten(playerId: Seq[String]) extends Command
  private case class FoodEaten(foodIds: Seq[String], newFood: Seq[Food]) extends Command
  private case class GameEnded(winnerId: String, winnerMass: Double) extends Command

  def apply(
      localPlayerId: String,
      world: World,
      speed: Double,
      winningMass: Int,
      isAIPlayer: Boolean
  ): Behavior[Command] =
    Behaviors.setup { context =>
      Behaviors.withTimers { timers =>
        context.log.info(s"Starting DistributedGameStateManager for player $localPlayerId")

        var localWorld = world
        var direction: Option[(Double, Double)] = None
        var otherManagers: Map[String, ActorRef[Command]] = Map.empty

        // Start internal timer for autonomous operation
        timers.startTimerWithFixedDelay("game-tick", InternalTick, 30.milliseconds)

        def updatePlayerPosition(world: World, playerId: String, dx: Double, dy: Double, speed: Double) =
          world.playerById(playerId) match {
            case Some(player) =>
              val newX = (player.x + dx * speed).max(0).min(world.width)
              val newY = (player.y + dy * speed).max(0).min(world.height)
              val updatedPlayer = player.copy(x = newX, y = newY)

              // Check for food consumption
              val foodEaten = world.foods.filter(food => EatingManager.canEatFood(updatedPlayer, food))
              // Generate new food for consumed food
              val newFoods = foodEaten.map { f =>
                Food(
                  id = java.util.UUID.randomUUID().toString,
                  x = Random.nextInt(world.width),
                  y = Random.nextInt(world.height),
                  mass = f.mass
                )
              }
              val playerAfterEatingFood = foodEaten.foldLeft(updatedPlayer)((p, food) => p.grow(food))

              // Check for player consumption
              val playersEaten = world
                .playersExcludingSelf(playerAfterEatingFood)
                .filter(otherPlayer => EatingManager.canEatPlayer(playerAfterEatingFood, otherPlayer))
              val finalPlayer = playersEaten.foldLeft(playerAfterEatingFood)((p, other) => p.grow(other))

              if (finalPlayer.mass >= winningMass) {
                context.self ! GameEnded(finalPlayer.id, finalPlayer.mass)
              }

              // Update the world with the new player state, removed players, and new food
              if (foodEaten.nonEmpty || playersEaten.nonEmpty) {
                context.log.info(otherManagers.keys.mkString("Managers: ", ", ", ""))
                context.log.info(s" total food size ${world.foods.size} \n food to remove ${foodEaten.size} \n new food size ${newFoods.size}")
                for (m <- otherManagers.values) {
                  if (foodEaten.nonEmpty) m ! FoodEaten(foodEaten.map(_.id), newFoods)
                  if (playersEaten.nonEmpty) m ! PlayerEaten(playersEaten.map(_.id))
                }
              }

              world
                .updatePlayer(finalPlayer)
                .removePlayers(playersEaten)
                .removeFoods(foodEaten)
                .addFoods(newFoods)
            case _ => world
          }

        def broadcastMovement(): Unit = {
          localWorld.playerById(localPlayerId).foreach { ourPlayer =>
            otherManagers.values.foreach { manager =>
              manager ! PlayerMovement(localPlayerId, ourPlayer.x, ourPlayer.y, ourPlayer.mass)
            }
          }
        }

        Behaviors.receiveMessage {
          case MovePlayer(id, dx, dy) if id == localPlayerId =>
            // Only handle movement for our own player
            direction = Some((dx, dy))
            Behaviors.same

          case GetWorld(replyTo) =>
            replyTo ! localWorld
            Behaviors.same

          case InternalTick =>
            if (isAIPlayer) {
              AIMovement.moveAI(localPlayerId, context.self)(context.system)
            }
            direction match {
              case Some((dx, dy)) =>
                localWorld = updatePlayerPosition(localWorld, localPlayerId, dx, dy, speed)
              case None =>
            }

            // Always broadcast our current position to other managers
            broadcastMovement()

            Behaviors.same

          case RegisterManager(managerId, manager) if managerId != localPlayerId =>
            context.log.info(s"Player $localPlayerId registering manager $managerId")
            otherManagers = otherManagers + (managerId -> manager)

            // Send our current player state to the newly registered manager
            localWorld.playerById(localPlayerId).foreach { ourPlayer =>
              manager ! PlayerMovement(localPlayerId, ourPlayer.x, ourPlayer.y, ourPlayer.mass)
            }

            // Only the first manager (or a designated manager) should send food state
            // to prevent food duplication when multiple managers register with a new one
            // We'll let the SyncWorldState message handle food synchronization instead

            Behaviors.same

          case PlayerMovement(id, x, y, mass) =>
            // Handle movement for any player, including our own (for synchronization)
            localWorld = localWorld.playerById(id) match {
              case Some(player) =>
                localWorld.updatePlayer(player.copy(x = x, y = y, mass = mass))
              case None =>
                context.log.warn(s"Received movement for unknown player $id")
                localWorld
            }
            Behaviors.same

          case PlayerEaten(ids) =>
            val playersToRemove = localWorld.players.filter(player => ids.contains(player.id))
            localWorld = localWorld.removePlayers(playersToRemove)
            Behaviors.same

          case FoodEaten(foodToRemoveIds, newFood) =>
            val foodToRemove = localWorld.foods.filter(food => foodToRemoveIds.contains(food.id))
            localWorld = localWorld.removeFoods(foodToRemove).addFoods(newFood)
            Behaviors.same

          case PlayerJoined(newPlayerId, player) =>
            context.log.info(s"Message from $localPlayerId Player $newPlayerId joined the game")
            localWorld = localWorld.addPlayer(player)
            Behaviors.same

          case SyncWorldState(worldState) =>
            context.log.info(s"Synchronizing world state with manager")
            localWorld = worldState
            // Optionally, notify this manager's player about the updated world state
            broadcastMovement()
            Behaviors.same

          case GameEnded(winnerId, winnerMass) =>
            // context.log.info(s"Game ended! Player $winnerId won with mass $winnerMass")
            // Don't stop the actor immediately - let the controller handle the shutdown
            // The controller will detect the game end and properly shut down all systems
            // TODO handle game end logic, e.g., notify UI or reset state
            Behaviors.same

          case PlayerLeft(leftPlayerId) =>
            if (leftPlayerId == localPlayerId) {
              context.log.info(s"Player $leftPlayerId left the game, shutting down manager")
              // Notify all other managers about this player's disconnection
              otherManagers.values.foreach { manager =>
                manager ! PlayerLeft(leftPlayerId)
              }
              import akka.actor.CoordinatedShutdown
              GameController.actorSystemTerminated(localPlayerId)
              CoordinatedShutdown(context.system).run(CoordinatedShutdown.clusterLeavingReason)
              // Stop processing further messages after initiating shutdown
              Behaviors.stopped
            } else {
              context.log.info(s"Player $leftPlayerId left the game, removing from world")
              localWorld = localWorld.removePlayer(leftPlayerId)
              Behaviors.same
            }
          case _ =>
            Behaviors.same
        }
      }
    }

def shutdownClusterSystem(system: ActorSystem): Unit = {
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  val cluster = Cluster(system)
  val coordinatedShutdown = CoordinatedShutdown(system)

  // Leave cluster first
  cluster.leave(cluster.selfAddress)

  // Register callback to terminate system after leaving cluster
  cluster.registerOnMemberRemoved {
    coordinatedShutdown.run(CoordinatedShutdown.ClusterLeavingReason)
  }

  // Wait for termination
  system.whenTerminated.onComplete {
    case Success(_) =>
      println("Actor system terminated and ports freed")
    case Failure(ex) =>
      println(s"Termination failed: $ex")
  }
}

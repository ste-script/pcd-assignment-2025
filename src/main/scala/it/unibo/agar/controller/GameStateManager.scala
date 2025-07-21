package it.unibo.agar.controller

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import it.unibo.agar.model.EatingManager
import it.unibo.agar.model.Food
import it.unibo.agar.model.Player
import it.unibo.agar.model.World
import it.unibo.agar.model.AIMovement
import akka.actor.CoordinatedShutdown
import akka.cluster.typed.Cluster
import akka.cluster.typed.Subscribe
import akka.cluster.ClusterEvent.MemberRemoved
import akka.cluster.ClusterEvent.MemberEvent
import akka.actor.Address

import scala.util.Random
import scala.concurrent.duration.*

object GameStateManager:

  sealed trait Command

  // Commands for player management
  case class MovePlayer(id: String, dx: Double, dy: Double) extends Command
  case class GetWorld(replyTo: ActorRef[World]) extends Command
  private case object InternalTick extends Command
  private case object DoNothing extends Command

  // Messages for inter-manager communication
  case class RegisterManager(managerId: String, manager: ActorSystem[Command]) extends Command
  case class PlayerJoined(playerId: String, player: Player) extends Command
  case class SyncWorldState(world: World) extends Command
  case class PlayerLeft(playerId: String) extends Command

  private case class PlayerMovement(playerId: String, x: Double, y: Double, mass: Double) extends Command
  private case class PlayerEaten(playerId: Seq[String]) extends Command
  private case class FoodEaten(foodIds: Seq[String], newFood: Seq[Food]) extends Command
  private case class GameEnded(winnerId: String, winnerMass: Double) extends Command
  private case class RemoveManager(address: Address) extends Command

  // Internal state holder
  private case class GameState(
      world: World,
      direction: Option[(Double, Double)] = None,
      otherManagers: Map[String, ActorSystem[Command]] = Map.empty
  )

  def apply(
      localPlayerId: String,
      world: World,
      speed: Double,
      winningMass: Int,
      isAIPlayer: Boolean
  ): Behavior[Command] =
    Behaviors.setup { context =>
      Behaviors.withTimers { timers =>
        val cluster = Cluster(context.system)
        val memberEventAdapter = context.messageAdapter[MemberEvent] {
          case MemberRemoved(member, _) => RemoveManager(member.address)
          case _ => DoNothing
        }
        cluster.subscriptions ! Subscribe(memberEventAdapter, classOf[MemberEvent])
        context.log.info(s"Starting GameStateManager for player $localPlayerId")

        var gameState = GameState(world)

        // Start internal timer for autonomous operation
        timers.startTimerWithFixedDelay("game-tick", InternalTick, 30.milliseconds)

        def constrainPosition(value: Double, max: Double): Double =
          value.max(0).min(max)

        def generateNewFood(consumedFood: Food, worldWidth: Int, worldHeight: Int): Food =
          Food(
            id = java.util.UUID.randomUUID().toString,
            x = Random.nextInt(worldWidth),
            y = Random.nextInt(worldHeight),
            mass = consumedFood.mass
          )

        def processEating(player: Player, world: World): (Player, Seq[Food], Seq[Player]) =
          val foodEaten = world.foods.filter(food => EatingManager.canEatFood(player, food))
          val playerAfterFood = foodEaten.foldLeft(player)((p, food) => p.grow(food))

          val playersEaten = world
            .playersExcludingSelf(playerAfterFood)
            .filter(otherPlayer => EatingManager.canEatPlayer(playerAfterFood, otherPlayer))
          val finalPlayer = playersEaten.foldLeft(playerAfterFood)((p, other) => p.grow(other))

          (finalPlayer, foodEaten, playersEaten)

        def notifyOtherManagers(foodEaten: Seq[Food], playersEaten: Seq[Player], newFoods: Seq[Food]): Unit =
          if (foodEaten.nonEmpty || playersEaten.nonEmpty) {
            gameState.otherManagers.values.foreach { manager =>
              if (foodEaten.nonEmpty) manager ! FoodEaten(foodEaten.map(_.id), newFoods)
              if (playersEaten.nonEmpty) manager ! PlayerEaten(playersEaten.map(_.id))
            }
          }

        def updatePlayerPosition(playerId: String, dx: Double, dy: Double): Unit =
          gameState.world.playerById(playerId) match {
            case Some(player) =>
              val newX = constrainPosition(player.x + dx * speed, gameState.world.width)
              val newY = constrainPosition(player.y + dy * speed, gameState.world.height)
              val updatedPlayer = player.copy(x = newX, y = newY)

              val (finalPlayer, foodEaten, playersEaten) = processEating(updatedPlayer, gameState.world)

              if (finalPlayer.mass >= winningMass) {
                context.self ! GameEnded(finalPlayer.id, finalPlayer.mass)
                gameState.otherManagers.values.foreach(_ ! GameEnded(finalPlayer.id, finalPlayer.mass))
              }

              val newFoods = foodEaten.map(generateNewFood(_, gameState.world.width, gameState.world.height))
              notifyOtherManagers(foodEaten, playersEaten, newFoods)

              gameState = gameState.copy(world =
                gameState.world
                  .updatePlayer(finalPlayer)
                  .removePlayers(playersEaten)
                  .removeFoods(foodEaten)
                  .addFoods(newFoods)
              )

            case None => // Player not found, do nothing
          }

        def broadcastMovement(): Unit =
          gameState.world.playerById(localPlayerId).foreach { ourPlayer =>
            gameState.otherManagers.values.foreach { manager =>
              manager ! PlayerMovement(localPlayerId, ourPlayer.x, ourPlayer.y, ourPlayer.mass)
            }
          }

        def handlePlayerMovement(id: String, x: Double, y: Double, mass: Double): Unit =
          gameState.world.playerById(id) match {
            case Some(player) =>
              gameState = gameState.copy(world = gameState.world.updatePlayer(player.copy(x = x, y = y, mass = mass)))
            case None =>
              context.log.warn(s"Received movement for unknown player $id")
          }

        def handlePlayerLeft(leftPlayerId: String): Behavior[Command] =
          if (leftPlayerId == localPlayerId) {
            context.log.info(s"Player $leftPlayerId left the game, shutting down manager")
            gameState.otherManagers.values.foreach(_ ! PlayerLeft(leftPlayerId))
            GameController.actorSystemTerminated(localPlayerId)
            CoordinatedShutdown(context.system).run(CoordinatedShutdown.clusterLeavingReason)
            Behaviors.stopped
          } else {
            context.log.info(s"Player $leftPlayerId left the game, removing from world")
            gameState = gameState.copy(
              world = gameState.world.removePlayer(leftPlayerId),
              otherManagers = gameState.otherManagers - leftPlayerId
            )
            Behaviors.same
          }

        Behaviors.receiveMessage {
          case MovePlayer(id, dx, dy) if id == localPlayerId =>
            gameState = gameState.copy(direction = Some((dx, dy)))
            Behaviors.same

          case GetWorld(replyTo) =>
            replyTo ! gameState.world
            Behaviors.same

          case InternalTick =>
            if (isAIPlayer) {
              AIMovement.moveAI(localPlayerId, context.self)(context.system)
            }

            gameState.direction.foreach { case (dx, dy) =>
              updatePlayerPosition(localPlayerId, dx, dy)
            }

            broadcastMovement()
            Behaviors.same

          case RegisterManager(managerId, manager) if managerId != localPlayerId =>
            context.log.info(s"Player $localPlayerId registering manager $managerId")
            gameState = gameState.copy(otherManagers = gameState.otherManagers + (managerId -> manager))

            gameState.world.playerById(localPlayerId).foreach { ourPlayer =>
              manager ! PlayerMovement(localPlayerId, ourPlayer.x, ourPlayer.y, ourPlayer.mass)
            }
            Behaviors.same

          case PlayerMovement(id, x, y, mass) =>
            handlePlayerMovement(id, x, y, mass)
            Behaviors.same

          case PlayerEaten(ids) =>
            val playersToRemove = gameState.world.players.filter(player => ids.contains(player.id))
            gameState = gameState.copy(world = gameState.world.removePlayers(playersToRemove))
            Behaviors.same

          case FoodEaten(foodToRemoveIds, newFood) =>
            val foodToRemove = gameState.world.foods.filter(food => foodToRemoveIds.contains(food.id))
            gameState = gameState.copy(world = gameState.world.removeFoods(foodToRemove).addFoods(newFood))
            Behaviors.same

          case PlayerJoined(newPlayerId, player) =>
            context.log.info(s"Player $newPlayerId joined the game")
            gameState = gameState.copy(world = gameState.world.addPlayer(player))
            Behaviors.same

          case SyncWorldState(worldState) =>
            context.log.info(s"Synchronizing world state")
            gameState = gameState.copy(world = worldState)
            broadcastMovement()
            Behaviors.same

          case GameEnded(winnerId, winnerMass) =>
            context.log.info(s"Game ended! Player $winnerId won with mass $winnerMass")
            Behaviors.same

          case PlayerLeft(leftPlayerId) =>
            handlePlayerLeft(leftPlayerId)

          case RemoveManager(address) =>
            val node = gameState.otherManagers.find((s, m) => m.address == address)
            if (node.isEmpty) {
              context.log.warn(s"Manager removing already handled: $address")
            } else {
              context.log.info(s"Removing node $node left the cluster without notifying, removing it")
              val playerIdToRemove = node.get._1
              context.self ! PlayerLeft(playerIdToRemove)
              gameState = gameState.copy(otherManagers = gameState.otherManagers - playerIdToRemove)
              context.log.info(
                s"Node $node with address $address left the cluster, removing associated player: $playerIdToRemove"
              )
            }
            Behaviors.same

          case _ =>
            Behaviors.same
        }
      }
    }

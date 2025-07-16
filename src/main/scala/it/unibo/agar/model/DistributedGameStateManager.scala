package it.unibo.agar.model

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import scala.util.Random

object DistributedGameStateManager:

  sealed trait Command

  // Commands for player management
  case class MovePlayer(id: String, dx: Double, dy: Double) extends Command
  case class GetWorld(replyTo: ActorRef[World]) extends Command
  case object Tick extends Command

  // Messages for inter-manager communication
  case class PlayerMovement(playerId: String, x: Double, y: Double, mass: Double) extends Command
  case class PlayerEaten(playerId: Seq[String]) extends Command
  case class FoodEaten(foodIds: Seq[String], newFood: Seq[Food]) extends Command
  case class RegisterManager(managerId: String, manager: ActorRef[Command]) extends Command
  case class PlayerJoined(playerId: String, player: Player) extends Command
  case class PlayerLeft(playerId: String) extends Command
  case class SyncWorldState(world: World) extends Command

  def apply(playerId: String, world: World, speed: Double = 10.0): Behavior[Command] =
    Behaviors.setup { context =>
      context.log.info(s"Starting DistributedGameStateManager for player $playerId")

      var localWorld = world
      var direction: Option[(Double, Double)] = None
      var otherManagers: Map[String, ActorRef[Command]] = Map.empty

      def updatePlayerPosition(world: World, playerId: String, dx: Double, dy: Double, speed: Double) =
        world.playerById(playerId) match {
          case Some(player) =>
            val newX = (player.x + dx * speed).max(0).min(world.width)
            val newY = (player.y + dy * speed).max(0).min(world.height)
            val updatedPlayer = player.copy(x = newX, y = newY)

            // Check for food consumption
            val foodEaten = world.foods.filter(food => EatingManager.canEatFood(updatedPlayer, food))
            val playerAfterEating = foodEaten.foldLeft(updatedPlayer)((p, food) => p.grow(food))

            // Check for player consumption
            val playersEaten = world
              .playersExcludingSelf(playerAfterEating)
              .filter(otherPlayer => EatingManager.canEatPlayer(playerAfterEating, otherPlayer))
            val finalPlayer = playersEaten.foldLeft(playerAfterEating)((p, other) => p.grow(other))

            // Generate new food for consumed food
            val newFoods = foodEaten.map { f =>
              Food(
                id = java.util.UUID.randomUUID().toString,
                x = Random.nextInt(world.width),
                y = Random.nextInt(world.height),
                mass = f.mass
              )
            }

            // Update the world with the new player state, removed players, and new food
            if (foodEaten.nonEmpty || playersEaten.nonEmpty) {
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
          case None => world
        }

      Behaviors.receiveMessage {
        case MovePlayer(id, dx, dy) if id == playerId =>
          // Only handle movement for our own player
          direction = Some((dx, dy))
          Behaviors.same

        case GetWorld(replyTo) =>
          replyTo ! localWorld
          Behaviors.same

        case Tick =>
          direction match {
            case Some((dx, dy)) =>
              localWorld = updatePlayerPosition(localWorld, playerId, dx, dy, speed)
            case None =>
          }

          // Always broadcast our current position to other managers
          localWorld.playerById(playerId).foreach { ourPlayer =>
            otherManagers.values.foreach { manager =>
              manager ! PlayerMovement(playerId, ourPlayer.x, ourPlayer.y, ourPlayer.mass)
            }
          }

          Behaviors.same

        case RegisterManager(managerId, manager) =>
          context.log.info(s"Player $playerId registering manager $managerId")
          otherManagers = otherManagers + (managerId -> manager)

          // Send our current player state to the newly registered manager
          localWorld.playerById(playerId).foreach { ourPlayer =>
            manager ! PlayerMovement(playerId, ourPlayer.x, ourPlayer.y, ourPlayer.mass)
          }

          // Send current world state (all players) to the new manager
          localWorld.players.foreach { player =>
            manager ! PlayerMovement(player.id, player.x, player.y, player.mass)
          }

          // Send current food state to the new manager
          manager ! FoodEaten(Seq.empty, localWorld.foods)

          Behaviors.same

        case PlayerMovement(id, x, y, mass) =>
          // Handle movement for any player, including our own (for synchronization)
          localWorld = localWorld.playerById(id) match {
            case Some(player) =>
              localWorld.updatePlayer(player.copy(x = x, y = y, mass = mass))
            case None =>
              val newPlayer = Player(id, x, y, mass)
              localWorld.copy(players = localWorld.players :+ newPlayer)
          }
          Behaviors.same

        case PlayerEaten(ids) =>
          val playersToRemove = localWorld.players.filter(player => ids.contains(player.id))
          localWorld = localWorld.removePlayers(playersToRemove)
          Behaviors.same

        case FoodEaten(foodIds, newFood) =>
          val foodToRemove = localWorld.foods.filter(food => foodIds.contains(food.id))
          localWorld = localWorld.removeFoods(foodToRemove).addFoods(newFood)
          Behaviors.same

        case PlayerJoined(newPlayerId, player) =>
          context.log.info(s"Player $newPlayerId joined the game")
          localWorld = localWorld.addPlayer(player)
          // Notify other managers about the new player
          otherManagers.values.foreach { manager =>
            manager ! PlayerMovement(player.id, player.x, player.y, player.mass)
          }
          Behaviors.same

        case PlayerLeft(leftPlayerId) =>
          context.log.info(s"Player $leftPlayerId left the game")
          localWorld = localWorld.removePlayer(leftPlayerId)
          // Notify other managers about the player leaving
          otherManagers.values.foreach { manager =>
            manager ! PlayerEaten(Seq(leftPlayerId))
          }
          Behaviors.same

        case SyncWorldState(worldState) =>
          context.log.info(s"Synchronizing world state with manager")
          localWorld = worldState
          // Optionally, notify this manager's player about the updated world state
          localWorld.playerById(playerId).foreach { ourPlayer =>
            otherManagers.values.foreach { manager =>
              manager ! PlayerMovement(playerId, ourPlayer.x, ourPlayer.y, ourPlayer.mass)
            }
          }
          Behaviors.same

        case _ =>
          Behaviors.same
      }
    }

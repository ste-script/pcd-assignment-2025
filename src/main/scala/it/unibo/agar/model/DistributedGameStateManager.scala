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
            for (m <- otherManagers.values) {
              m ! FoodEaten(foodEaten.map(_.id), newFoods)
              m ! PlayerEaten(playersEaten.map(_.id))
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
          otherManagers = otherManagers + (managerId -> manager)

          // Send our current player state to the newly registered manager
          localWorld.playerById(playerId).foreach { ourPlayer =>
            manager ! PlayerMovement(playerId, ourPlayer.x, ourPlayer.y, ourPlayer.mass)
          }

          Behaviors.same

        case PlayerMovement(id, x, y, mass) if id != playerId =>
          // Always update or add the player - this ensures players persist
          localWorld = localWorld.playerById(id) match {
            case Some(player) =>
              localWorld.updatePlayer(player.copy(x = x, y = y, mass = mass))
            case None =>
              val newPlayer = Player(id, x, y, mass)
              localWorld.copy(players = localWorld.players :+ newPlayer)
          }
          Behaviors.same

        case PlayerEaten(ids) =>
          val playerToRemove = localWorld.players.filter(player => ids.contains(player.id))
          localWorld = localWorld.removePlayers(playerToRemove)
          Behaviors.same

        case FoodEaten(foodIds, newFood) =>
          val foodToRemove = localWorld.foods.filter(food => foodIds.contains(food.id))
          localWorld = localWorld.removeFoods(foodToRemove).addFoods(newFood)
          Behaviors.same

        case _ =>
          Behaviors.same
      }

    }

package it.unibo.agar.model

import akka.actor.typed.ActorRef
import akka.util.Timeout
import it.unibo.agar.controller.GameStateActor

import scala.concurrent.duration.*
import scala.concurrent.Future
import scala.util.{Failure, Success}

/** Object responsible for AI movement logic, separate from the game state management */
object AIMovement:

  implicit val timeout: Timeout = 3.seconds

  /** Finds the nearest food for a given player in the world
    * @param player
    *   the ID of the player for whom to find the nearest food
    * @param world
    *   the current game world containing players and food
    * @return
    */
  private def nearestFood(player: String, world: World): Option[Food] =
    world.foods
      .sortBy(food => world.playerById(player).map(p => p.distanceTo(food)).getOrElse(Double.MaxValue))
      .headOption

  /** Moves the AI toward the nearest food using actor message passing
    *
    * @param name
    *   The name/ID of the AI player
    * @param gameManager
    *   The actor reference to the game state manager
    * @param system
    *   The actor system for scheduling
    */
  def moveAI(name: String, gameManager: ActorRef[GameStateActor.Command])(implicit system: akka.actor.typed.ActorSystem[_]): Unit =
    import akka.actor.typed.scaladsl.AskPattern._

    // Request the current world state from the game manager
    val worldFuture: Future[World] = gameManager.ask(GameStateActor.GetWorld.apply)

    worldFuture.onComplete {
      case Success(world) =>
        val aiOpt = world.playerById(name)
        val foodOpt = nearestFood(name, world)
        (aiOpt, foodOpt) match
          case (Some(ai), Some(food)) =>
            val dx = food.x - ai.x
            val dy = food.y - ai.y
            val distance = math.hypot(dx, dy)

            // Add minimum distance threshold to prevent oscillation
            val minDistance = 5.0 // Adjust based on your game scale
            val moveSpeed = 0.5   // Reduce speed to prevent overshooting

            if (distance > minDistance) {
              val normalizedDx = (dx / distance) * moveSpeed
              val normalizedDy = (dy / distance) * moveSpeed
              gameManager ! GameStateActor.MovePlayer(name, normalizedDx, normalizedDy)
            }
          // If very close to food, stop moving to prevent oscillation

          case (Some(ai), None) =>
            // No food available - implement random movement or stay still
            val randomDx = (math.random() - 0.5) * 0.1
            val randomDy = (math.random() - 0.5) * 0.1
            gameManager ! GameStateActor.MovePlayer(name, randomDx, randomDy)

          case _ => // Do nothing if AI doesn't exist
      case Failure(exception) =>
        println(s"Failed to get world state for AI $name: ${exception.getMessage}")
    }(system.executionContext)

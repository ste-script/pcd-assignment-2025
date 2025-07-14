package it.unibo.agar.model

import akka.actor.typed.ActorRef
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Failure}

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
  def nearestFood(player: String, world: World): Option[Food] =
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
  def moveAI(name: String, gameManager: ActorRef[DistributedGameStateManager.Command])(implicit system: akka.actor.typed.ActorSystem[_]): Unit =
    import akka.actor.typed.scaladsl.AskPattern._
    
    // Request the current world state from the game manager
    val worldFuture: Future[World] = gameManager.ask(DistributedGameStateManager.GetWorld.apply)
    
    worldFuture.onComplete {
      case Success(world) =>
        val aiOpt = world.playerById(name)
        val foodOpt = nearestFood(name, world)
        (aiOpt, foodOpt) match
          case (Some(ai), Some(food)) =>
            val dx = food.x - ai.x
            val dy = food.y - ai.y
            val distance = math.hypot(dx, dy)
            if (distance > 0)
              val normalizedDx = dx / distance
              val normalizedDy = dy / distance
              // Send movement command to the game manager
              gameManager ! DistributedGameStateManager.MovePlayer(name, normalizedDx, normalizedDy)
          case _ => // Do nothing if AI or food doesn't exist
      case Failure(exception) =>
        // Handle timeout or other errors
        println(s"Failed to get world state for AI $name: ${exception.getMessage}")
    }(system.executionContext)

package it.unibo.agar.model

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

object GameCoordinator:
  sealed trait Command
  
  case class RegisterPlayerManager(playerId: String, manager: ActorRef[DistributedGameStateManager.Command]) extends Command
  case class GetManagerForPlayer(playerId: String, replyTo: ActorRef[Option[ActorRef[DistributedGameStateManager.Command]]]) extends Command
  case object StartGame extends Command
  case object Tick extends Command
  
  def apply(): Behavior[Command] =
    Behaviors.setup { context =>
      context.log.info("Starting GameCoordinator")
      
      def active(playerManagers: Map[String, ActorRef[DistributedGameStateManager.Command]]): Behavior[Command] =
        Behaviors.receiveMessage {
          case RegisterPlayerManager(playerId, manager) =>
            context.log.info(s"Registering manager for player $playerId")
            val updatedManagers = playerManagers + (playerId -> manager)
            
            // Register this manager with all other managers
            playerManagers.values.foreach { otherManager =>
              otherManager ! DistributedGameStateManager.RegisterManager(playerId, manager)
            }
            
            // Register all other managers with this new manager
            playerManagers.foreach { case (otherPlayerId, otherManager) =>
              manager ! DistributedGameStateManager.RegisterManager(otherPlayerId, otherManager)
            }
            
            active(updatedManagers)
            
          case GetManagerForPlayer(playerId, replyTo) =>
            replyTo ! playerManagers.get(playerId)
            Behaviors.same
            
          case StartGame =>
            context.log.info("Starting game with coordinated managers")
            Behaviors.same
            
          case Tick =>
            // Send tick to all player managers
            playerManagers.values.foreach { manager =>
              manager ! DistributedGameStateManager.Tick
            }
            Behaviors.same
        }
      
      active(Map.empty)
    }

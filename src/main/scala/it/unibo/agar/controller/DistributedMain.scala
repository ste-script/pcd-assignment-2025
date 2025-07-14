package it.unibo.agar.controller

import akka.actor.typed.{ActorRef, ActorSystem}
import akka.actor.typed.scaladsl.Behaviors
import it.unibo.agar.model.{AIMovement, DistributedGameStateManager, GameCoordinator, GameInitializer, World}
import it.unibo.agar.view.{GlobalView, LocalView}

import java.awt.Window
import java.util.Timer
import java.util.TimerTask
import scala.swing.*
import scala.swing.Swing.onEDT

object DistributedMain extends SimpleSwingApplication:

  private val width = 1000
  private val height = 1000
  private val numPlayers = 4
  private val numFoods = 100
  private val players = GameInitializer.initialPlayers(numPlayers, width, height)
  private val foods = GameInitializer.initialFoods(numFoods, width, height)
  private val initialWorld = World(width, height, players, foods)

  // Create the coordinator
  private val coordinatorBehavior = GameCoordinator()
  private val coordinator: ActorSystem[GameCoordinator.Command] =
    it.unibo.agar.startup("agario-coordinator", 25251)(coordinatorBehavior)

  // Create individual game state managers for each player
  private val playerManagers: Map[String, ActorRef[DistributedGameStateManager.Command]] =
    players.map { player =>
      // Each player starts with only their own player in the world, plus all the food
      val playerWorld = World(width, height, Seq(player), foods)
      val managerBehavior = DistributedGameStateManager(player.id, playerWorld)
      val manager = coordinator.systemActorOf(managerBehavior, s"manager-${player.id}")

      // Register the manager with the coordinator
      coordinator ! GameCoordinator.RegisterPlayerManager(player.id, manager)

      player.id -> manager
    }.toMap

  // Use the coordinator as the actor system for implicit parameters
  implicit val system: ActorSystem[GameCoordinator.Command] = coordinator

  private val timer = new Timer()
  private val task: TimerTask = new TimerTask:
    override def run(): Unit =
      // Move AI for player p1 using its dedicated manager
      playerManagers.get("p1").foreach { manager =>
        AIMovement.moveAI("p1", manager)(coordinator)
      }

      // Send tick to coordinator which will distribute to all managers
      coordinator ! GameCoordinator.Tick
      onEDT(Window.getWindows.foreach(_.repaint()))
  timer.scheduleAtFixedRate(task, 0, 30) // every 30ms

  override def top: Frame =
    // Add a longer delay to ensure all managers have time to register and exchange player information
    Thread.sleep(500)

    // Use p1's manager for global view (it should have received updates from all other managers)
    val globalManager = playerManagers("p1")
    new GlobalView(globalManager)(coordinator).open()

    playerManagers.get("p1").foreach { manager =>
      new LocalView(manager, "p1")(coordinator).open()
    }

    playerManagers.get("p2").foreach { manager =>
      new LocalView(manager, "p2")(coordinator).open()
    }

    // No launcher window, just return an empty frame
    new Frame { visible = false }
package it.unibo.agar.view

import akka.actor.typed.ActorRef
import akka.util.Timeout
import it.unibo.agar.controller.GameStateActor
import it.unibo.agar.model.World
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Failure}

import java.awt.Color
import java.awt.Graphics2D
import scala.swing.*
import java.awt.event.WindowEvent
import java.awt.event.WindowAdapter

class GlobalView(manager: ActorRef[GameStateActor.Command])(implicit system: akka.actor.typed.ActorSystem[_]) extends MainFrame:

  implicit val timeout: Timeout = 3.seconds
  private var currentWorld: World = World(800, 800, Seq.empty, Seq.empty) // Default empty world
  private var isShuttingDown: Boolean = false

  title = "Agar.io - Global View"
  preferredSize = new Dimension(800, 800)

  contents = new Panel:
    override def paintComponent(g: Graphics2D): Unit =
      AgarViewUtils.drawWorld(g, currentWorld)

  // Request world updates periodically
  private val timer = new javax.swing.Timer(30, _ => updateWorld())
  timer.start()

  // Add window closing listener to properly shutdown the timer
  peer.addWindowListener(new WindowAdapter {
    override def windowClosing(e: WindowEvent): Unit = {
      shutdown()
    }
  })

  private def updateWorld(): Unit =
    // Check if we're shutting down or the actor system is terminated
    if (isShuttingDown || system.whenTerminated.isCompleted) {
      return
    }

    import akka.actor.typed.scaladsl.AskPattern._

    try {
      val worldFuture: Future[World] = manager.ask(GameStateActor.GetWorld.apply)
      worldFuture.onComplete {
        case Success(world) =>
          if (!isShuttingDown) {
            currentWorld = world
            repaint()
          }
        case Failure(exception) =>
          if (!isShuttingDown && !system.whenTerminated.isCompleted) {
            println(s"Failed to get world state for GlobalView: ${exception.getMessage}")
          }
      }(system.executionContext)
    } catch {
      case _: IllegalStateException => 
        // Actor system is already shut down, stop trying to make calls
        shutdown()
    }

  def shutdown(): Unit = {
    if (!isShuttingDown) {
      isShuttingDown = true
      if (timer != null) {
        timer.stop()
      }
    }
  }

  override def closeOperation(): Unit = {
    shutdown()
    super.closeOperation()
  }

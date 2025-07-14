package it.unibo.agar.view

import akka.actor.typed.ActorRef
import akka.util.Timeout
import it.unibo.agar.model.{DistributedGameStateManager, World}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Failure}

import java.awt.Color
import java.awt.Graphics2D
import scala.swing.*

class GlobalView(manager: ActorRef[DistributedGameStateManager.Command])(implicit system: akka.actor.typed.ActorSystem[_]) extends MainFrame:

  implicit val timeout: Timeout = 3.seconds
  private var currentWorld: World = World(800, 800, Seq.empty, Seq.empty) // Default empty world

  title = "Agar.io - Global View"
  preferredSize = new Dimension(800, 800)

  contents = new Panel:
    override def paintComponent(g: Graphics2D): Unit =
      AgarViewUtils.drawWorld(g, currentWorld)

  // Request world updates periodically
  private val timer = new javax.swing.Timer(30, _ => updateWorld())
  timer.start()

  private def updateWorld(): Unit =
    import akka.actor.typed.scaladsl.AskPattern._

    val worldFuture: Future[World] = manager.ask(DistributedGameStateManager.GetWorld.apply)
    worldFuture.onComplete {
      case Success(world) =>
        currentWorld = world
        repaint()
      case Failure(exception) =>
        println(s"Failed to get world state for GlobalView: ${exception.getMessage}")
    }(system.executionContext)

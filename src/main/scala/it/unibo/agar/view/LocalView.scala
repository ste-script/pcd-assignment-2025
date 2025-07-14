package it.unibo.agar.view

import akka.actor.typed.ActorRef
import akka.util.Timeout
import it.unibo.agar.model.{DistributedGameStateManager, World}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Failure}

import java.awt.Graphics2D
import scala.swing.*

class LocalView(manager: ActorRef[DistributedGameStateManager.Command], playerId: String)(implicit system: akka.actor.typed.ActorSystem[_]) extends MainFrame:

  implicit val timeout: Timeout = 3.seconds
  private var currentWorld: World = World(400, 400, Seq.empty, Seq.empty) // Default empty world

  title = s"Agar.io - Local View ($playerId)"
  preferredSize = new Dimension(400, 400)

  contents = new Panel:
    listenTo(keys, mouse.moves)
    focusable = true
    requestFocusInWindow()

    override def paintComponent(g: Graphics2D): Unit =
      val playerOpt = currentWorld.players.find(_.id == playerId)
      val (offsetX, offsetY) = playerOpt
        .map(p => (p.x - size.width / 2.0, p.y - size.height / 2.0))
        .getOrElse((0.0, 0.0))
      AgarViewUtils.drawWorld(g, currentWorld, offsetX, offsetY)

    reactions += { case e: event.MouseMoved =>
      val mousePos = e.point
      val playerOpt = currentWorld.players.find(_.id == playerId)
      playerOpt.foreach: player =>
        val dx = (mousePos.x - size.width / 2) * 0.01
        val dy = (mousePos.y - size.height / 2) * 0.01
        manager ! DistributedGameStateManager.MovePlayer(playerId, dx, dy)
      repaint()
    }

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
        println(s"Failed to get world state for LocalView ($playerId): ${exception.getMessage}")
    }(system.executionContext)

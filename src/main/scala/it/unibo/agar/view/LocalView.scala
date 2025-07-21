package it.unibo.agar.view

import akka.actor.typed.ActorRef
import akka.util.Timeout
import it.unibo.agar.controller.GameStateManager
import it.unibo.agar.model.World
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Success
import scala.util.Failure

import java.awt.Graphics2D
import scala.swing.*
import java.awt.event.WindowEvent
import java.awt.event.WindowAdapter

class LocalView(
    manager: ActorRef[GameStateManager.Command],
    playerId: String
)(implicit system: akka.actor.typed.ActorSystem[_])
    extends MainFrame:

  implicit val timeout: Timeout = 3.seconds
  private var currentWorld: World = World(400, 400, Seq.empty, Seq.empty) // Default empty world
  private var isShuttingDown: Boolean = false

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
      if (!isShuttingDown && !system.whenTerminated.isCompleted) {
        val mousePos = e.point
        val playerOpt = currentWorld.players.find(_.id == playerId)
        playerOpt.foreach: player =>
          val dx = (mousePos.x - size.width / 2) * 0.01
          val dy = (mousePos.y - size.height / 2) * 0.01
          manager ! GameStateManager.MovePlayer(playerId, dx, dy)
        repaint()
      }
    }

  // Request world updates periodically
  private val timer = new javax.swing.Timer(30, _ => updateWorld())
  timer.start()

  private def updateWorld(): Unit =
    // Check if we're shutting down or the actor system is terminated
    if (isShuttingDown || system.whenTerminated.isCompleted) {
      return
    }

    import akka.actor.typed.scaladsl.AskPattern._

    try {
      val worldFuture: Future[World] = manager.ask(GameStateManager.GetWorld.apply)
      worldFuture.onComplete {
        case Success(world) =>
          if (!isShuttingDown) {
            currentWorld = world
            if(currentWorld.playerWon.isDefined){
              // Player has won, draw a message
                val winner = currentWorld.playerWon.get
                if(winner._1 == playerId) {
                  contents = new Label(s"You won with mass: ${winner._2}") {
                    horizontalAlignment = Alignment.Center
                    preferredSize = new Dimension(400, 400)
                  }
                } else {
                  contents = new Label(s"${winner._1} won with mass: ${winner._2}") {
                    horizontalAlignment = Alignment.Center
                    preferredSize = new Dimension(400, 400)
                  }
                }
            }
            repaint()
          }
        case Failure(exception) =>
          if (!isShuttingDown && !system.whenTerminated.isCompleted) {
            println(s"Failed to get world state for LocalView ($playerId): ${exception.getMessage}")
          }
      }(system.executionContext)
    } catch {
      case _: IllegalStateException =>
        // Actor system is already shut down, stop trying to make calls
        shutdown()
    }

  private def notifyDisconnection(): Unit = {
    manager ! GameStateManager.PlayerLeft(playerId)
    system.log.info(s"Player $playerId is disconnecting")
  }

  def shutdown(): Unit = {
    if (!isShuttingDown) {
      isShuttingDown = true
      notifyDisconnection() // Notify the game system about disconnection
      if (timer != null) {
        timer.stop()
      }
    }
  }

  override def closeOperation(): Unit =
    shutdown()

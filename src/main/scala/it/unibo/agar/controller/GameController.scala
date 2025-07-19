package it.unibo.agar.controller

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Scheduler
import it.unibo.agar.model.AIMovement
import it.unibo.agar.model.GameInitializer
import it.unibo.agar.model.Player
import it.unibo.agar.model.World
import it.unibo.agar.view.GlobalView
import it.unibo.agar.view.JoinGameRequest
import it.unibo.agar.view.LocalView

import java.awt.Window
import java.util.Timer
import java.util.TimerTask
import scala.swing.Swing.onEDT
import scala.util.Random
import javax.swing.JOptionPane
import scala.concurrent.ExecutionContextExecutor
import akka.actor.typed.scaladsl.AskPattern.*
import akka.util.Timeout

import scala.concurrent.duration.*
import scala.util.Success
import scala.util.Failure

object GameController {

  private val width = 1000
  private val height = 1000
  private val numFoods = 100
  private val winningMass = 1000
  private val playerSpeed = 10
  private val initialNetworkPort = 25251

  private var gameTimer: Timer = null
  private var isGameActive = false
  private var gameEnded = false
  private var winner: Option[(String, Double)] = None

  // Store information about running games
  private var activeSystems: Map[String, ActorSystem[GameStateManager.Command]] = Map.empty
  private var aiPlayers: Set[String] = Set.empty

  // Track views for proper shutdown
  private var globalView: Option[GlobalView] = None
  private var localViews: Map[String, LocalView] = Map.empty

  def startEmptyGame(): Unit = {
    println("Starting empty game - ready for players to join")

    // Initialize empty world with just food
    val emptyWorld = World(width, height, Seq.empty, GameInitializer.initialFoods(numFoods, width, height))

    // Create a base system to maintain the world state
    val baseSystem = it.unibo.agar.startup("agario", initialNetworkPort)(
      GameStateManager("__server__", emptyWorld, playerSpeed, winningMass)
    )

    // Store the base system
    activeSystems += ("__server__" -> baseSystem)

    // Mark game as active
    isGameActive = true

    // Start game timer for world updates
    startGameTimer()

    // Create and open global view with the empty game
    implicit val implicitSystem: ActorSystem[GameStateManager.Command] = baseSystem
    globalView = Some(new GlobalView(baseSystem))
    globalView.foreach(_.open())

    println("Empty game started successfully - players can now join")
  }

  def joinExistingGame(joinRequest: JoinGameRequest): Unit = {
    if (!isGameActive) {
      showError("No active game found. Please start a new game first.")
      return
    }

    println(s"Player ${joinRequest.playerName} attempting to join existing game")

    // Check if player name already exists
    if (activeSystems.contains(joinRequest.playerName)) {
      showError(s"Player name '${joinRequest.playerName}' already exists in the game.")
      return
    }

    try {
      // Create new player
      val newPlayer = it.unibo.agar.model.Player(
        id = joinRequest.playerName,
        x = Random.nextInt(width).toDouble,
        y = Random.nextInt(height).toDouble,
        mass = 120.0
      )

      // Get current world state from an existing system
      if (activeSystems.values.headOption.isEmpty) {
        showError("Unable to connect to existing game.")
        return
      }

      // Create new system for the joining player
      val nextPort = initialNetworkPort + activeSystems.size
      val newSystem = createPlayerSystem(newPlayer, nextPort)

      // Add to active systems
      activeSystems += (newPlayer.id -> newSystem)
      // Register the new system with all existing systems
      registerNewPlayerWithExistingSystems(newPlayer, newSystem)

      // Track AI players
      if (joinRequest.isAI) {
        aiPlayers += newPlayer.id
      }

      // Create view for human player
      if (!joinRequest.isAI) {
        implicit val implicitSystem: ActorSystem[GameStateManager.Command] = newSystem
        new LocalView(newSystem, newPlayer.id).open()
      }

      println(s"Player ${joinRequest.playerName} successfully joined the game")

    } catch {
      case e: Exception =>
        showError(s"Failed to join game: ${e.getMessage}")
    }
  }

  private def createPlayerSystem(
      player: it.unibo.agar.model.Player,
      port: Int
  ): ActorSystem[GameStateManager.Command] = {
    // Joining player - create minimal world without food, will be synchronized later
    val managerBehavior =
      GameStateManager(player.id, World(width, height, Seq(player), Seq.empty), playerSpeed, winningMass)
    it.unibo.agar.startup("agario", port)(managerBehavior)
  }

  private def registerNewPlayerWithExistingSystems(
      newPlayer: Player,
      newSystem: ActorSystem[GameStateManager.Command]
  ): Unit = {

    // Get current world state from an existing system to synchronize the new player
    if (activeSystems.nonEmpty) {
      val existingSystem = activeSystems.values.head

      // Use ask pattern to get current world state

      implicit val timeout: Timeout = 3.seconds
      implicit val scheduler: Scheduler = existingSystem.scheduler
      implicit val ec: ExecutionContextExecutor = existingSystem.executionContext

      val worldFuture = existingSystem ? GameStateManager.GetWorld.apply
      worldFuture.onComplete {
        case Success(currentWorld) =>
          // Sync the new system with current world state
          newSystem ! GameStateManager.SyncWorldState(currentWorld)

          // Notify all existing systems about the new player
          for ((_, existingSystem) <- activeSystems)
            existingSystem ! GameStateManager.PlayerJoined(newPlayer.id, newPlayer)

          println(s"Successfully synchronized new player ${newPlayer.id}  with existing game state")

        case Failure(exception) =>
          println(s"Failed to synchronize new player ${newPlayer.id}: ${exception.getMessage}")
      }
    }

    // Register new player with all existing systems
    for ((existingPlayerId, existingSystem) <- activeSystems) {
      // Register new player with existing system
      existingSystem ! GameStateManager.RegisterManager(newPlayer.id, newSystem)

      // Register existing player with new system
      newSystem ! GameStateManager.RegisterManager(existingPlayerId, existingSystem)
    }
  }

  private def startGameTimer(): Unit = {
    if (gameTimer != null) {
      gameTimer.cancel()
    }

    gameTimer = new Timer()
    val task: TimerTask = new TimerTask {
      override def run(): Unit = {
        if (!gameEnded) {
          // Move AI players
          for (aiPlayerId <- aiPlayers) {
            activeSystems.get(aiPlayerId).foreach { aiSystem =>
              AIMovement.moveAI(aiPlayerId, aiSystem)(aiSystem)
            }
          }

          // Send tick to all active systems
          for (system <- activeSystems.values)
            system ! GameStateManager.Tick

          // Update UI
          onEDT(Window.getWindows.foreach(_.repaint()))
        }
      }
    }

    gameTimer.scheduleAtFixedRate(task, 0, 30) // every 30ms
  }

  private def showError(message: String): Unit = {
    JOptionPane.showMessageDialog(
      null,
      message,
      "Error",
      JOptionPane.ERROR_MESSAGE
    )
  }

}

package it.unibo.agar.controller

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Scheduler
import it.unibo.agar.model.AIMovement
import it.unibo.agar.model.GameInitializer
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

import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.util.Success
import scala.util.Failure

object GameController {

  private val width = 1000
  private val height = 1000
  private val numFoods = 100
  private val winningMass = 1000
  private val playerSpeed = 10

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
    val baseSystem = it.unibo.agar.startup("agario", 25250)(
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
      val nextPort = 25250 + activeSystems.size
      val newSystem = createPlayerSystem(newPlayer, nextPort)

      // Register the new system with all existing systems
      registerNewPlayerWithExistingSystems(newPlayer.id, newSystem)

      // Add to active systems
      activeSystems += (newPlayer.id -> newSystem)

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
      port: Int,
      isInitialGameSetup: Boolean = false
  ): ActorSystem[GameStateManager.Command] = {

    // For initial game setup, all players get full food
    // For players joining existing games, create minimal world without food
    val world = if (isInitialGameSetup) {
      // Initial game setup - create world with initial food for all players
      World(width, height, Seq(player), GameInitializer.initialFoods(numFoods, width, height))
    } else {
      // Joining player - create minimal world without food, will be synchronized later
      World(width, height, Seq(player), Seq.empty)
    }

    val managerBehavior = GameStateManager(player.id, world, playerSpeed, winningMass)
    it.unibo.agar.startup("agario", port)(managerBehavior)
  }

  private def registerNewPlayerWithExistingSystems(
      newPlayerId: String,
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

          // Create new player object
          val newPlayer = it.unibo.agar.model.Player(
            id = newPlayerId,
            x = scala.util.Random.nextInt(width).toDouble,
            y = scala.util.Random.nextInt(height).toDouble,
            mass = 120.0
          )

          // Notify all existing systems about the new player
          for ((_, existingSystem) <- activeSystems)
            existingSystem ! GameStateManager.PlayerJoined(newPlayerId, newPlayer)

          println(s"Successfully synchronized new player $newPlayerId with existing game state")

        case Failure(exception) =>
          println(s"Failed to synchronize new player $newPlayerId: ${exception.getMessage}")
      }
    }

    // Register new player with all existing systems
    for ((existingPlayerId, existingSystem) <- activeSystems) {
      // Register new player with existing system
      existingSystem ! GameStateManager.RegisterManager(newPlayerId, newSystem)

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

package it.unibo.agar.controller

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Scheduler
import it.unibo.agar.model.AIMovement
import it.unibo.agar.model.DistributedGameStateManager
import it.unibo.agar.model.GameInitializer
import it.unibo.agar.model.World
import it.unibo.agar.view.GlobalView
import it.unibo.agar.view.JoinGameRequest
import it.unibo.agar.view.LocalView
import it.unibo.agar.view.PlayerInfo

import java.awt.Window
import java.util.Timer
import java.util.TimerTask
import scala.swing.Swing.onEDT
import scala.util.Random
import javax.swing.JOptionPane
import scala.concurrent.ExecutionContextExecutor

object DistributedGameController {

  private val width = 1000
  private val height = 1000
  private val numFoods = 100

  private var gameTimer: Timer = null
  private var isGameActive = false

  // Store information about running games
  private var activeSystems: Map[String, ActorSystem[DistributedGameStateManager.Command]] = Map.empty
  private var aiPlayers: Set[String] = Set.empty

  def startNewGame(playerInfos: List[PlayerInfo]): Unit = {
    println(s"Starting new distributed game with ${playerInfos.length} players")

    // Create initial players
    val players = playerInfos.map { info =>
      it.unibo.agar.model.Player(
        id = info.name,
        x = Random.nextInt(width).toDouble,
        y = Random.nextInt(height).toDouble,
        mass = 120.0
      )
    }

    // Track AI players
    aiPlayers = playerInfos.filter(_.isAI).map(_.name).toSet

    // Create and start game systems
    startGameSystems(players)

    // Mark game as active
    isGameActive = true

    // Create views for human players
    createInitialViews(players)
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
      val existingSystem = activeSystems.values.headOption
      if (existingSystem.isEmpty) {
        showError("Unable to connect to existing game.")
        return
      }

      // Create new system for the joining player
      val nextPort = 25251 + activeSystems.size
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
        implicit val implicitSystem: ActorSystem[DistributedGameStateManager.Command] = newSystem
        new LocalView(newSystem, newPlayer.id).open()
      }

      println(s"Player ${joinRequest.playerName} successfully joined the game")

    } catch {
      case e: Exception =>
        showError(s"Failed to join game: ${e.getMessage}")
    }
  }

  private def startGameSystems(players: Seq[it.unibo.agar.model.Player]): Unit = {
    var systems: List[ActorSystem[DistributedGameStateManager.Command]] = List.empty

    // Create ActorSystems for each player
    for ((player, index) <- players.zipWithIndex) {
      val system = createPlayerSystem(player, 25251 + index)
      systems = systems :+ system
      activeSystems += (player.id -> system)
    }

    // Register all systems with each other
    registerAllSystems(players)

    // Start game timer
    startGameTimer()
  }

  private def createPlayerSystem(
      player: it.unibo.agar.model.Player,
      port: Int,
  ): ActorSystem[DistributedGameStateManager.Command] = {

    // Use existing world or create a minimal one for joining players
    val world = World(width, height, Seq(player), GameInitializer.initialFoods(numFoods, width, height))

    val managerBehavior = DistributedGameStateManager(player.id, world)
    it.unibo.agar.startup("agario", port)(managerBehavior)
  }

  private def registerAllSystems(players: Seq[it.unibo.agar.model.Player]): Unit = {
    for ((player, index) <- players.zipWithIndex) {
      val currentSystem = activeSystems(player.id)
      for ((otherPlayer, otherIndex) <- players.zipWithIndex if otherIndex != index) {
        val otherSystem = activeSystems(otherPlayer.id)
        println(s"Registering ${player.id} with ${otherPlayer.id}")
        currentSystem ! DistributedGameStateManager.RegisterManager(otherPlayer.id, otherSystem)
      }
    }

  }

  private def registerNewPlayerWithExistingSystems(
      newPlayerId: String,
      newSystem: ActorSystem[DistributedGameStateManager.Command]
  ): Unit = {

    // Get current world state from an existing system to synchronize the new player
    if (activeSystems.nonEmpty) {
      val existingSystem = activeSystems.values.head

      // Use ask pattern to get current world state
      import akka.actor.typed.scaladsl.AskPattern._
      import akka.util.Timeout
      import scala.concurrent.duration._
      import scala.util.{Success, Failure}

      implicit val timeout: Timeout = 3.seconds
      implicit val scheduler: Scheduler = existingSystem.scheduler
      implicit val ec: ExecutionContextExecutor = existingSystem.executionContext

      val worldFuture = existingSystem ? DistributedGameStateManager.GetWorld.apply
      worldFuture.onComplete {
        case Success(currentWorld) =>
          // Sync the new system with current world state
          newSystem ! DistributedGameStateManager.SyncWorldState(currentWorld)

          // Create new player object
          val newPlayer = it.unibo.agar.model.Player(
            id = newPlayerId,
            x = scala.util.Random.nextInt(width).toDouble,
            y = scala.util.Random.nextInt(height).toDouble,
            mass = 120.0
          )

          // Notify all existing systems about the new player
          for ((_, existingSystem) <- activeSystems)
            existingSystem ! DistributedGameStateManager.PlayerJoined(newPlayerId, newPlayer)

          println(s"Successfully synchronized new player $newPlayerId with existing game state")

        case Failure(exception) =>
          println(s"Failed to synchronize new player $newPlayerId: ${exception.getMessage}")
      }
    }

    // Register new player with all existing systems
    for ((existingPlayerId, existingSystem) <- activeSystems) {
      // Register new player with existing system
      existingSystem ! DistributedGameStateManager.RegisterManager(newPlayerId, newSystem)

      // Register existing player with new system
      newSystem ! DistributedGameStateManager.RegisterManager(existingPlayerId, existingSystem)
    }
  }

  private def startGameTimer(): Unit = {
    if (gameTimer != null) {
      gameTimer.cancel()
    }

    gameTimer = new Timer()
    val task: TimerTask = new TimerTask {
      override def run(): Unit = {
        // Move AI players
        for (aiPlayerId <- aiPlayers) {
          activeSystems.get(aiPlayerId).foreach { aiSystem =>
            AIMovement.moveAI(aiPlayerId, aiSystem)(aiSystem)
          }
        }

        // Send tick to all active systems
        for (system <- activeSystems.values)
          system ! DistributedGameStateManager.Tick

        // Update UI
        onEDT(Window.getWindows.foreach(_.repaint()))
      }
    }

    gameTimer.scheduleAtFixedRate(task, 0, 30) // every 30ms
  }

  private def createInitialViews(players: Seq[it.unibo.agar.model.Player]): Unit = {
    // Wait for systems to be ready
    if (activeSystems.nonEmpty) {
      // Create global view using first system
      val globalSystem = activeSystems.values.head
      implicit val implicitSystem: ActorSystem[DistributedGameStateManager.Command] = globalSystem
      new GlobalView(globalSystem).open()

      // Create local views for human players
      for (player <- players) {
        if (!aiPlayers.contains(player.id)) {
          activeSystems.get(player.id).foreach { playerSystem =>
            new LocalView(playerSystem, player.id).open()
          }
        }
      }
    }
  }

  private def showError(message: String): Unit = {
    JOptionPane.showMessageDialog(
      null,
      message,
      "Error",
      JOptionPane.ERROR_MESSAGE
    )
  }

  def shutdown(): Unit = {
    if (gameTimer != null) {
      gameTimer.cancel()
    }

    activeSystems.values.foreach(_.terminate())
    activeSystems = Map.empty
    aiPlayers = Set.empty
    isGameActive = false
  }

  def isActive: Boolean = isGameActive
  def getActivePlayers: Set[String] = activeSystems.keySet

}

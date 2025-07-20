package it.unibo.agar.controller

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Scheduler
import akka.actor.typed.scaladsl.AskPattern.*
import akka.util.Timeout
import it.unibo.agar.model.GameInitializer
import it.unibo.agar.model.Player
import it.unibo.agar.model.World
import it.unibo.agar.view.GlobalView
import it.unibo.agar.view.JoinGameRequest
import it.unibo.agar.view.LocalView

import javax.swing.JOptionPane
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.*
import scala.util.Failure
import scala.util.Random
import scala.util.Success

object GameController:

  // Game configuration
  private object Config:

    val width = 800
    val height = 800
    val numFoods = 100
    val winningMass = 1000
    val playerSpeed = 10
    val serverPlayerId = "__server__"
    val basePort = 25251
    val syncTimeout: FiniteDuration = 3.seconds

  // Game state management
  private case class GameState(
      isActive: Boolean = false,
      gameEnded: Boolean = false,
      winner: Option[(String, Double)] = None,
      activeSystems: Map[String, ActorSystem[GameStateManager.Command]] = Map.empty,
      baseSystem: Option[ActorSystem[GameStateManager.Command]] = None,
      globalView: Option[GlobalView] = None,
      localViews: Map[String, LocalView] = Map.empty
  )

  private var gameState = GameState()

  def startEmptyGame(): Unit =
    println("Starting empty game - ready for players to join")

    val emptyWorld = createEmptyWorld()
    val baseSystem = createBaseSystem(emptyWorld)

    gameState = gameState.copy(
      isActive = true,
      activeSystems = gameState.activeSystems + (Config.serverPlayerId -> baseSystem),
      baseSystem = Some(baseSystem)
    )

    initializeGlobalView(baseSystem)
    println("Empty game started successfully - players can now join")

  def joinExistingGame(joinRequest: JoinGameRequest): Unit =
    if (!gameState.isActive) {
      showError("No active game found. Please start a new game first.")
      return
    }

    if (isPlayerNameTaken(joinRequest.playerName)) {
      showError(s"Player name '${joinRequest.playerName}' already exists in the game.")
      return
    }

    println(s"Player ${joinRequest.playerName} attempting to join existing game")

    try {
      val newPlayer = createNewPlayer(joinRequest.playerName)
      val newSystem = createPlayerSystem(newPlayer, joinRequest.isAI)

      addPlayerToGame(newPlayer, newSystem, joinRequest.isAI)
      registerNewPlayerWithExistingSystems(newPlayer, newSystem)

      println(s"Player ${joinRequest.playerName} successfully joined the game")
    } catch {
      case e: Exception =>
        showError(s"Failed to join game: ${e.getMessage}")
    }

  def actorSystemTerminated(systemId: String): Unit =
    gameState = gameState.copy(
      activeSystems = gameState.activeSystems.filterNot(_._1 == systemId),
      localViews = gameState.localViews.filterNot(_._1 == systemId)
    )

  // Private helper methods
  private def createEmptyWorld(): World =
    World(
      Config.width,
      Config.height,
      Seq.empty,
      GameInitializer.initialFoods(Config.numFoods, Config.width, Config.height)
    )

  private def createBaseSystem(world: World): ActorSystem[GameStateManager.Command] =
    it.unibo.agar.startup("agario", Config.basePort)(
      GameStateManager(Config.serverPlayerId, world, Config.playerSpeed, Config.winningMass, false)
    )

  private def initializeGlobalView(baseSystem: ActorSystem[GameStateManager.Command]): Unit =
    implicit val implicitSystem: ActorSystem[GameStateManager.Command] = baseSystem
    val globalView = new GlobalView(baseSystem)
    globalView.open()
    gameState = gameState.copy(globalView = Some(globalView))

  private def isPlayerNameTaken(playerName: String): Boolean =
    gameState.activeSystems.contains(playerName)

  private def createNewPlayer(playerId: String): Player =
    Player(
      id = playerId,
      x = Random.nextInt(Config.width).toDouble,
      y = Random.nextInt(Config.height).toDouble,
      mass = 120.0
    )

  private def createPlayerSystem(player: Player, isAI: Boolean): ActorSystem[GameStateManager.Command] =
    val managerBehavior = GameStateManager(
      player.id,
      World(Config.width, Config.height, Seq(player), Seq.empty),
      Config.playerSpeed,
      Config.winningMass,
      isAI
    )
    it.unibo.agar.startup("agario")(managerBehavior)

  private def addPlayerToGame(player: Player, system: ActorSystem[GameStateManager.Command], isAI: Boolean): Unit =
    gameState = gameState.copy(
      activeSystems = gameState.activeSystems + (player.id -> system)
    )

    if (!isAI) {
      createLocalViewForPlayer(player, system)
    }

  private def createLocalViewForPlayer(player: Player, system: ActorSystem[GameStateManager.Command]): Unit =
    implicit val implicitSystem: ActorSystem[GameStateManager.Command] = system
    val localView = new LocalView(system, player.id)
    localView.open()
    gameState = gameState.copy(
      localViews = gameState.localViews + (player.id -> localView)
    )

  private def registerNewPlayerWithExistingSystems(
      newPlayer: Player,
      newSystem: ActorSystem[GameStateManager.Command]
  ): Unit =
    syncNewPlayerWithGameState(newPlayer, newSystem)
    crossRegisterSystems(newPlayer, newSystem)

  private def syncNewPlayerWithGameState(newPlayer: Player, newSystem: ActorSystem[GameStateManager.Command]): Unit =
    gameState.baseSystem.foreach { existingSystem =>
      implicit val timeout: Timeout = Config.syncTimeout
      implicit val scheduler: Scheduler = existingSystem.scheduler
      implicit val ec: ExecutionContextExecutor = existingSystem.executionContext

      val worldFuture = existingSystem ? GameStateManager.GetWorld.apply
      worldFuture.onComplete {
        case Success(currentWorld) =>
          newSystem ! GameStateManager.SyncWorldState(currentWorld)
          notifyExistingSystemsOfNewPlayer(newPlayer)
          println(s"Successfully synchronized new player ${newPlayer.id} with existing game state")

        case Failure(exception) =>
          println(s"Failed to synchronize new player ${newPlayer.id}: ${exception.getMessage}")
      }
    }

  private def notifyExistingSystemsOfNewPlayer(newPlayer: Player): Unit =
    gameState.activeSystems.values.foreach { system =>
      system ! GameStateManager.PlayerJoined(newPlayer.id, newPlayer)
    }

  private def crossRegisterSystems(newPlayer: Player, newSystem: ActorSystem[GameStateManager.Command]): Unit =
    gameState.activeSystems.foreach { case (existingPlayerId, existingSystem) =>
      existingSystem ! GameStateManager.RegisterManager(newPlayer.id, newSystem)
      newSystem ! GameStateManager.RegisterManager(existingPlayerId, existingSystem)
    }

  private def showError(message: String): Unit =
    JOptionPane.showMessageDialog(
      null,
      message,
      "Error",
      JOptionPane.ERROR_MESSAGE
    )

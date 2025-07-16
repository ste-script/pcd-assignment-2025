package it.unibo.agar.controller

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import it.unibo.agar.model.AIMovement
import it.unibo.agar.model.DistributedGameStateManager
import it.unibo.agar.model.GameInitializer
import it.unibo.agar.model.World
import it.unibo.agar.view.GlobalView
import it.unibo.agar.view.LocalView

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

  var systems = List.empty[ActorSystem[DistributedGameStateManager.Command]]
  var counter = 0

  // Create ActorSystems for each player
  for (player <- players) {
    val managerBehavior = DistributedGameStateManager(player.id, initialWorld)
    val system = it.unibo.agar.startup("agario", 25251 + counter)(managerBehavior)
    systems = systems :+ system
    counter += 1
  }

  // Register all managers with each other using proper ActorRef references
  for ((player, index) <- players.zipWithIndex) do
    val currentSystem = systems(index)
    for ((otherPlayer, otherIndex) <- players.zipWithIndex if otherIndex != index) do
      val otherSystem = systems(otherIndex)
      println(s"Registering ${player.id} with ${otherPlayer.id}")
      currentSystem ! DistributedGameStateManager.RegisterManager(otherPlayer.id, otherSystem)

  // Wait for registration to complete

  // Use the first system as the implicit system
  implicit val system: ActorSystem[DistributedGameStateManager.Command] = systems.head

  private val timer = new Timer()
  private val task: TimerTask = new TimerTask:
    override def run(): Unit =
      // Move AI for player p1 using its dedicated manager (first system)
      val p1System = systems.head
      AIMovement.moveAI("p1", p1System)(p1System)

      // Send a Tick message to each manager to update the game state
      for (managerSystem <- systems) do managerSystem ! DistributedGameStateManager.Tick
      onEDT(Window.getWindows.foreach(_.repaint()))
  timer.scheduleAtFixedRate(task, 0, 30) // every 30ms

  override def top: Frame =
    // Add a longer delay to ensure all managers have time to register and exchange player information
    // Use p1's manager for global view (first system)
    val globalManager = systems.head
    new GlobalView(globalManager)(system).open()

    // Create LocalView for each player using their corresponding manager
    for ((player, index) <- players.zipWithIndex)
      val playerManager = systems(index)
      new LocalView(playerManager, player.id)(system).open()

    // No launcher window, just return an empty frame
    new Frame { visible = false }

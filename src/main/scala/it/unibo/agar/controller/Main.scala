package it.unibo.agar.controller

import it.unibo.agar.model.AIMovement
import it.unibo.agar.model.GameInitializer
import it.unibo.agar.model.MockGameStateManager
import it.unibo.agar.model.World
import it.unibo.agar.view.GlobalView
import it.unibo.agar.view.LocalView

import java.awt.Window
import java.util.Timer
import java.util.TimerTask
import scala.swing.*
import scala.swing.Swing.onEDT

object Main extends SimpleSwingApplication:

  private val width = 1000
  private val height = 1000
  private val numPlayers = 4
  private val numFoods = 100
  private val players = GameInitializer.initialPlayers(numPlayers, width, height)
  private val foods = GameInitializer.initialFoods(numFoods, width, height)
  private val manager = new MockGameStateManager(World(width, height, players, foods))

  private val timer = new Timer()
  private val task: TimerTask = new TimerTask:
    override def run(): Unit =
      AIMovement.moveAI("p1", manager)
      manager.tick()
      onEDT(Window.getWindows.foreach(_.repaint()))
  timer.scheduleAtFixedRate(task, 0, 30) // every 30ms

  override def top: Frame =
    // Open both views at startup
    new GlobalView(manager).open()
    new LocalView(manager, "p1").open()
    new LocalView(manager, "p2").open()
    // No launcher window, just return an empty frame (or null if allowed)
    new Frame { visible = false }

package it.unibo.agar.controller

import it.unibo.agar.view.{PlayerInfo}
import scala.swing.*

object DistributedMain extends SimpleSwingApplication:

  override def top: Frame = {
    // Redirect to the new GameLauncher
    GameLauncher.top
  }

  override def main(args: Array[String]): Unit = {
    // Start the game launcher
    GameLauncher.main(args)
  }

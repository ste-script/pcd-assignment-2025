package it.unibo.agar.controller

import scala.swing.*
import it.unibo.agar.view.JoinGameView
import it.unibo.agar.view.LoginView
import it.unibo.agar.view.MainMenuView

object GameLauncher extends SimpleSwingApplication {

  // Start the game automatically with no players when the launcher starts
  GameController.startEmptyGame()

  override def top: Frame =
    MainMenuView(joinExistingGame)

  private def joinExistingGame(): Unit = {
    val joinView = new JoinGameView()

    joinView.setOnJoinRequest { joinRequest =>
      GameController.joinExistingGame(joinRequest)
    }

    joinView.open()
  }

}

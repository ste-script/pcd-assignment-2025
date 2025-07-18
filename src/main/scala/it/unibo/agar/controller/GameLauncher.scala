package it.unibo.agar.controller

import scala.swing.*
import it.unibo.agar.view.JoinGameView
import it.unibo.agar.view.LoginView
import it.unibo.agar.view.MainMenuView

object GameLauncher extends SimpleSwingApplication {

  override def top: Frame =
    MainMenuView(startNewGame, joinExistingGame)

  private def startNewGame(): Unit = {
    val loginView = new LoginView()
    loginView.addDefaultPlayers()

    loginView.setOnPlayersReady { playerInfos =>
      DistributedGameManager.startNewGame(playerInfos)
    }

    loginView.open()
  }

  private def joinExistingGame(): Unit = {
    val joinView = new JoinGameView()

    joinView.setOnJoinRequest { joinRequest =>
      DistributedGameManager.joinExistingGame(joinRequest)
    }

    joinView.open()
  }

}

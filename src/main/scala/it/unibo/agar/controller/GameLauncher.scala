package it.unibo.agar.controller

import scala.swing.*
import scala.swing.event.*
import java.awt.{Color, Font}
import javax.swing.BorderFactory
import it.unibo.agar.view.{LoginView, JoinGameView, PlayerInfo, JoinGameRequest}

object GameLauncher extends SimpleSwingApplication {
  
  override def top: Frame = {
    val launcherFrame = new MainFrame {
      title = "Agar.io - Game Launcher"
      preferredSize = new Dimension(400, 250)
      resizable = false
      
      val mainPanel = new BoxPanel(Orientation.Vertical) {
        border = BorderFactory.createEmptyBorder(40, 40, 40, 40)
      }
      
      // Title
      val titleLabel = new Label("Agar.io Distributed Game") {
        font = new Font("Arial", Font.BOLD, 24)
        horizontalAlignment = Alignment.Center
      }
      
      // Buttons
      val newGameButton = new Button("Start New Game") {
        font = new Font("Arial", Font.BOLD, 16)
        preferredSize = new Dimension(200, 40)
        background = new Color(0, 150, 0)
        foreground = Color.WHITE
      }
      
      val joinGameButton = new Button("Join Existing Game") {
        font = new Font("Arial", Font.BOLD, 16)
        preferredSize = new Dimension(200, 40)
        background = new Color(0, 100, 200)
        foreground = Color.WHITE
      }
      
      // Event handlers
      newGameButton.reactions += {
        case ButtonClicked(_) => startNewGame()
      }
      
      joinGameButton.reactions += {
        case ButtonClicked(_) => joinExistingGame()
      }
      
      // Layout
      mainPanel.contents += titleLabel
      mainPanel.contents += Swing.VStrut(30)
      mainPanel.contents += newGameButton
      mainPanel.contents += Swing.VStrut(15)
      mainPanel.contents += joinGameButton
      
      contents = mainPanel
    }
    
    launcherFrame
  }
  
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

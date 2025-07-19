package it.unibo.agar.view

import scala.swing.*
import scala.swing.event.*
import javax.swing.BorderFactory
import java.awt.{Color, Font}

object MainMenuView {

  def apply(joinExistingGame: () => Unit): MainFrame = new MainFrame {
    title = "Agar.io - Game Launcher"
    preferredSize = new Dimension(400, 250)
    resizable = false

    val mainPanel: BoxPanel = new BoxPanel(Orientation.Vertical) {
      border = BorderFactory.createEmptyBorder(40, 40, 40, 40)
    }

    // Title
    val titleLabel: Label = new Label("Agar.io Distributed Game") {
      font = new Font("Arial", Font.BOLD, 24)
      horizontalAlignment = Alignment.Center
    }

    // Join Game Button
    val joinGameButton: Button = new Button("Join Game") {
      font = new Font("Arial", Font.BOLD, 16)
      preferredSize = new Dimension(200, 40)
      background = new Color(0, 100, 200)
      foreground = Color.WHITE
    }

    // Event handler
    joinGameButton.reactions += { case ButtonClicked(_) =>
      joinExistingGame()
    }

    // Layout
    mainPanel.contents += titleLabel
    mainPanel.contents += Swing.VStrut(30)
    mainPanel.contents += joinGameButton

    contents = mainPanel
  }

}

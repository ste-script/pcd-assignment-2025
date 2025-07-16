package it.unibo.agar.view

import scala.swing.*
import scala.swing.event.*
import java.awt.{Color, Font}
import javax.swing.{BorderFactory, JOptionPane}

case class JoinGameRequest(playerName: String, isAI: Boolean, gamePort: Int)

class JoinGameView extends MainFrame {
  
  private var onJoinRequest: JoinGameRequest => Unit = _ => ()
  
  title = "Agar.io - Join Game"
  preferredSize = new Dimension(400, 300)
  resizable = false
  
  // UI Components
  private val playerNameField = new TextField {
    columns = 15
    font = new Font("Arial", Font.PLAIN, 14)
  }
  
  private val isAICheckbox = new CheckBox("AI Player") {
    font = new Font("Arial", Font.PLAIN, 12)
  }
  
  private val gamePortField = new TextField {
    columns = 10
    font = new Font("Arial", Font.PLAIN, 14)
    text = "25251" // Default game port
  }
  
  private val joinButton = new Button("Join Game") {
    font = new Font("Arial", Font.BOLD, 14)
    preferredSize = new Dimension(120, 30)
    background = new Color(0, 150, 0)
    foreground = Color.WHITE
  }
  
  private val cancelButton = new Button("Cancel") {
    font = new Font("Arial", Font.PLAIN, 14)
    preferredSize = new Dimension(120, 30)
  }
  
  // Layout
  setupLayout()
  
  // Event handlers
  setupEventHandlers()
  
  private def setupLayout(): Unit = {
    val mainPanel = new BoxPanel(Orientation.Vertical) {
      border = BorderFactory.createEmptyBorder(30, 30, 30, 30)
    }
    
    // Title
    val titleLabel = new Label("Join Existing Game") {
      font = new Font("Arial", Font.BOLD, 18)
      horizontalAlignment = Alignment.Center
    }
    
    // Player name input
    val namePanel = new BoxPanel(Orientation.Horizontal) {
      contents += new Label("Player Name: ") {
        font = new Font("Arial", Font.PLAIN, 12)
        preferredSize = new Dimension(100, 25)
      }
      contents += Swing.HStrut(10)
      contents += playerNameField
    }
    
    // AI checkbox
    val aiPanel = new BoxPanel(Orientation.Horizontal) {
      contents += isAICheckbox
      contents += Swing.HGlue
    }
    
    // Game port input
    val portPanel = new BoxPanel(Orientation.Horizontal) {
      contents += new Label("Game Port: ") {
        font = new Font("Arial", Font.PLAIN, 12)
        preferredSize = new Dimension(100, 25)
      }
      contents += Swing.HStrut(10)
      contents += gamePortField
    }
    
    // Buttons
    val buttonPanel = new BoxPanel(Orientation.Horizontal) {
      contents += joinButton
      contents += Swing.HStrut(10)
      contents += cancelButton
    }
    
    // Assembly
    mainPanel.contents += titleLabel
    mainPanel.contents += Swing.VStrut(20)
    mainPanel.contents += namePanel
    mainPanel.contents += Swing.VStrut(15)
    mainPanel.contents += aiPanel
    mainPanel.contents += Swing.VStrut(15)
    mainPanel.contents += portPanel
    mainPanel.contents += Swing.VStrut(30)
    mainPanel.contents += buttonPanel
    
    contents = mainPanel
  }
  
  private def setupEventHandlers(): Unit = {
    // Join button
    joinButton.reactions += {
      case ButtonClicked(_) => attemptJoin()
    }
    
    // Cancel button
    cancelButton.reactions += {
      case ButtonClicked(_) => dispose()
    }
    
    // Enter key handlers
    playerNameField.reactions += {
      case KeyPressed(_, Key.Enter, _, _) => attemptJoin()
    }
    
    gamePortField.reactions += {
      case KeyPressed(_, Key.Enter, _, _) => attemptJoin()
    }
  }
  
  private def attemptJoin(): Unit = {
    val name = playerNameField.text.trim
    val portText = gamePortField.text.trim
    
    if (name.isEmpty) {
      JOptionPane.showMessageDialog(
        this.peer,
        "Please enter a player name",
        "Invalid Input",
        JOptionPane.WARNING_MESSAGE
      )
      return
    }
    
    val port = try {
      portText.toInt
    } catch {
      case _: NumberFormatException =>
        JOptionPane.showMessageDialog(
          this.peer,
          "Please enter a valid port number",
          "Invalid Port",
          JOptionPane.WARNING_MESSAGE
        )
        return
    }
    
    if (port < 1024 || port > 65535) {
      JOptionPane.showMessageDialog(
        this.peer,
        "Port must be between 1024 and 65535",
        "Invalid Port Range",
        JOptionPane.WARNING_MESSAGE
      )
      return
    }
    
    val joinRequest = JoinGameRequest(name, isAICheckbox.selected, port)
    onJoinRequest(joinRequest)
    dispose()
  }
  
  def setOnJoinRequest(callback: JoinGameRequest => Unit): Unit = {
    onJoinRequest = callback
  }
}

package it.unibo.agar.view

import scala.swing.*
import scala.swing.event.*
import java.awt.{Color, Font}
import javax.swing.{BorderFactory, JOptionPane}

case class PlayerInfo(name: String, isAI: Boolean)

class LoginView extends MainFrame {
  
  private var players: List[PlayerInfo] = List.empty
  private var onPlayersReady: List[PlayerInfo] => Unit = _ => ()
  
  title = "Agar.io - Player Setup"
  preferredSize = new Dimension(500, 600)
  resizable = false
  
  // UI Components
  private val playerNameField = new TextField {
    columns = 15
    font = new Font("Arial", Font.PLAIN, 14)
  }
  
  private val isAICheckbox = new CheckBox("AI Player") {
    font = new Font("Arial", Font.PLAIN, 12)
  }
  
  private val addPlayerButton = new Button("Add Player") {
    font = new Font("Arial", Font.BOLD, 14)
    preferredSize = new Dimension(120, 30)
  }
  
  private val removePlayerButton = new Button("Remove Selected") {
    font = new Font("Arial", Font.PLAIN, 12)
    preferredSize = new Dimension(120, 30)
    enabled = false
  }
  
  private val playersListModel = new javax.swing.DefaultListModel[String]()
  private val playersList = new ListView[String] {
    listData = Seq.empty
    font = new Font("Arial", Font.PLAIN, 12)
    selection.intervalMode = ListView.IntervalMode.Single
  }
  
  private val startGameButton = new Button("Start Game") {
    font = new Font("Arial", Font.BOLD, 16)
    preferredSize = new Dimension(150, 40)
    enabled = false
    background = new Color(0, 150, 0)
    foreground = Color.WHITE
  }
  
  private val minPlayersLabel = new Label("Minimum 2 players required") {
    font = new Font("Arial", Font.ITALIC, 11)
    foreground = Color.RED
  }
  
  // Layout
  setupLayout()
  
  // Event handlers
  setupEventHandlers()
  
  private def setupLayout(): Unit = {
    val mainPanel = new BoxPanel(Orientation.Vertical) {
      border = BorderFactory.createEmptyBorder(20, 20, 20, 20)
    }
    
    // Title
    val titleLabel = new Label("Agar.io - Multiplayer Setup") {
      font = new Font("Arial", Font.BOLD, 20)
      horizontalAlignment = Alignment.Center
    }
    
    // Player input section
    val inputPanel = new BoxPanel(Orientation.Vertical) {
      border = BorderFactory.createTitledBorder("Add Player")
    }
    
    val namePanel = new BoxPanel(Orientation.Horizontal) {
      contents += new Label("Player Name: ") {
        font = new Font("Arial", Font.PLAIN, 12)
      }
      contents += Swing.HStrut(10)
      contents += playerNameField
    }
    
    val aiPanel = new BoxPanel(Orientation.Horizontal) {
      contents += isAICheckbox
      contents += Swing.HGlue
    }
    
    val buttonPanel = new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += addPlayerButton
      contents += Swing.HGlue
    }
    
    inputPanel.contents += Swing.VStrut(10)
    inputPanel.contents += namePanel
    inputPanel.contents += Swing.VStrut(10)
    inputPanel.contents += aiPanel
    inputPanel.contents += Swing.VStrut(10)
    inputPanel.contents += buttonPanel
    inputPanel.contents += Swing.VStrut(10)
    
    // Players list section
    val listPanel = new BoxPanel(Orientation.Vertical) {
      border = BorderFactory.createTitledBorder("Players")
    }
    
    val scrollPane = new ScrollPane(playersList) {
      preferredSize = new Dimension(400, 200)
    }
    
    val listButtonPanel = new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += removePlayerButton
      contents += Swing.HGlue
    }
    
    listPanel.contents += scrollPane
    listPanel.contents += Swing.VStrut(10)
    listPanel.contents += listButtonPanel
    
    // Start game section
    val startPanel = new BoxPanel(Orientation.Vertical) {
      contents += Swing.VStrut(20)
      contents += minPlayersLabel
      contents += Swing.VStrut(10)
      contents += startGameButton
    }
    
    // Assembly
    mainPanel.contents += titleLabel
    mainPanel.contents += Swing.VStrut(20)
    mainPanel.contents += inputPanel
    mainPanel.contents += Swing.VStrut(20)
    mainPanel.contents += listPanel
    mainPanel.contents += Swing.VStrut(20)
    mainPanel.contents += startPanel
    
    contents = mainPanel
  }
  
  private def setupEventHandlers(): Unit = {
    // Add player button
    addPlayerButton.reactions += {
      case ButtonClicked(_) => addPlayer()
    }
    
    // Remove player button
    removePlayerButton.reactions += {
      case ButtonClicked(_) => removeSelectedPlayer()
    }
    
    // Start game button
    startGameButton.reactions += {
      case ButtonClicked(_) => startGame()
    }
    
    // Enter key in text field
    playerNameField.reactions += {
      case KeyPressed(_, Key.Enter, _, _) => addPlayer()
    }
    
    // List selection changes
    playersList.selection.reactions += {
      case ListSelectionChanged(_, _, _) => 
        removePlayerButton.enabled = playersList.selection.indices.nonEmpty
    }
  }
  
  private def addPlayer(): Unit = {
    val name = playerNameField.text.trim
    
    if (name.isEmpty) {
      JOptionPane.showMessageDialog(
        this.peer,
        "Please enter a player name",
        "Invalid Input",
        JOptionPane.WARNING_MESSAGE
      )
      return
    }
    
    if (players.exists(_.name.equalsIgnoreCase(name))) {
      JOptionPane.showMessageDialog(
        this.peer,
        "Player name already exists",
        "Duplicate Name",
        JOptionPane.WARNING_MESSAGE
      )
      return
    }
    
    if (players.length >= 8) {
      JOptionPane.showMessageDialog(
        this.peer,
        "Maximum 8 players allowed",
        "Too Many Players",
        JOptionPane.WARNING_MESSAGE
      )
      return
    }
    
    val playerInfo = PlayerInfo(name, isAICheckbox.selected)
    players = players :+ playerInfo
    
    updatePlayersList()
    
    // Clear input fields
    playerNameField.text = ""
    isAICheckbox.selected = false
    playerNameField.requestFocus()
    
    updateUI()
  }
  
  private def removeSelectedPlayer(): Unit = {
    playersList.selection.indices.headOption.foreach { index =>
      players = players.patch(index, Nil, 1)
      updatePlayersList()
      updateUI()
    }
  }
  
  private def updatePlayersList(): Unit = {
    val listData = players.map { player =>
      val typeStr = if (player.isAI) " (AI)" else " (Human)"
      player.name + typeStr
    }
    playersList.listData = listData
  }
  
  private def updateUI(): Unit = {
    val hasEnoughPlayers = players.length >= 2
    startGameButton.enabled = hasEnoughPlayers
    minPlayersLabel.visible = !hasEnoughPlayers
    
    if (hasEnoughPlayers) {
      minPlayersLabel.text = s"${players.length} players ready"
      minPlayersLabel.foreground = new Color(0, 100, 0)
    } else {
      minPlayersLabel.text = "Minimum 2 players required"
      minPlayersLabel.foreground = Color.RED
    }
  }
  
  private def startGame(): Unit = {
    if (players.length < 2) {
      JOptionPane.showMessageDialog(
        this.peer,
        "At least 2 players are required to start the game",
        "Not Enough Players",
        JOptionPane.WARNING_MESSAGE
      )
      return
    }
    
    onPlayersReady(players)
    dispose()
  }
  
  def setOnPlayersReady(callback: List[PlayerInfo] => Unit): Unit = {
    onPlayersReady = callback
  }
  
  // Add some default players for testing
  def addDefaultPlayers(): Unit = {
    players = List(
      PlayerInfo("Player 1", isAI = false),
      PlayerInfo("AI Bot", isAI = true)
    )
    updatePlayersList()
    updateUI()
  }
}

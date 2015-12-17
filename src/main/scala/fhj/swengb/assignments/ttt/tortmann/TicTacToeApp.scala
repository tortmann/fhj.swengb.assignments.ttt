package fhj.swengb.assignments.ttt.tortmann


import javafx.application.Application
import javafx.event.EventHandler
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.{Label, Button}
import javafx.scene.input.MouseEvent
import javafx.scene.{Scene, Parent}
import javafx.stage.Stage
import scala.util.control.NonFatal

object TicTacToeApp {
  def main(args: Array[String]) {
    Application.launch(classOf[TicTacToeApp], args:_*)
  }
}

class TicTacToeApp extends javafx.application.Application {

  val fxmlMain = "/fhj/swengb/assignments/ttt/TicTacToeApp.fxml"
  val cssMain = "/fhj/swengb/assignments/ttt/ttt.css"

  val loader = new FXMLLoader(getClass.getResource(fxmlMain))

  def setSkin(stage: Stage, fxml: String, css: String): Boolean = {
    val scene = new Scene(loader.load[Parent]())
    stage.setScene(scene)
    stage.getScene.getStylesheets.clear()
    stage.getScene.getStylesheets.add(css)
  }

  override def start(stage: Stage): Unit =
    try {
      stage.setTitle("Administrative Section")
      loader.load[Parent]() // side effect
      val scene = new Scene(loader.getRoot[Parent])
      stage.setScene(scene)
      stage.getScene.getStylesheets.add(cssMain)
      stage.show()
    } catch {
      case NonFatal(e) => e.printStackTrace()
    }

}

class TicTacToeAppController {
  @FXML private var btn_0: Button = _
  @FXML private var btn_1: Button = _
  @FXML private var btn_2: Button = _
  @FXML private var btn_3: Button = _
  @FXML private var btn_4: Button = _
  @FXML private var btn_5: Button = _
  @FXML private var btn_6: Button = _
  @FXML private var btn_7: Button = _
  @FXML private var btn_8: Button = _
  @FXML private var btn_new: Button = _

  val buttons: List[Button] = List(btn_0, btn_1, btn_2, btn_3, btn_4, btn_5, btn_6, btn_7, btn_8)

  def initialize(): Unit = {}

  def restart(): Unit = {
    startGame = TicTacToe()
    btn_0.setText("");btn_1.setText("");btn_2.setText("");btn_3.setText("");btn_4.setText("");btn_5.setText("");btn_6.setText("");btn_7.setText("");btn_8.setText("")
    btn_0.setDisable(false);btn_1.setDisable(false);btn_2.setDisable(false);btn_3.setDisable(false);btn_4.setDisable(false);btn_5.setDisable(false);btn_6.setDisable(false);btn_7.setDisable(false);btn_8.setDisable(false)
  }
  var startGame = TicTacToe()

  def updateGame(field:Button, currentgame:TicTacToe): TicTacToe = {
    field.setText(if (currentgame.moveHistory.size % 2 == 0) "x" else "o")
    val getButtonId = field.getId
    val position:TMove = getButtonId match {
      case "btn_0" => TopLeft
      case "btn_1" => TopCenter
      case "btn_2" => TopRight
      case "btn_3" => MiddleLeft
      case "btn_4" => MiddleCenter
      case "btn_5" => MiddleRight
      case "btn_6" => BottomLeft
      case "btn_7" => BottomCenter
      case "btn_8" => BottomRight
    }
    val gameUpToDate = currentgame.turn(position, currentgame.nextPlayer)
    field.setDisable(true)
    if (gameUpToDate.gameOver) {
      btn_0.setDisable(true);btn_1.setDisable(true);btn_2.setDisable(true);btn_3.setDisable(true);btn_4.setDisable(true);btn_5.setDisable(true);btn_6.setDisable(true);btn_7.setDisable(true);btn_8.setDisable(true)
    }
    gameUpToDate
  }

  def B0(): Unit = {
    val gameUpToDate = updateGame(btn_0,startGame); startGame = gameUpToDate
  }
  def B1(): Unit = {
    val gameUpToDate = updateGame(btn_1,startGame); startGame = gameUpToDate
  }
  def B2(): Unit = {
    val gameUpToDate = updateGame(btn_2,startGame); startGame = gameUpToDate
  }
  def B3(): Unit = {
    val gameUpToDate = updateGame(btn_3,startGame); startGame = gameUpToDate
  }
  def B4(): Unit = {
    val gameUpToDate = updateGame(btn_4,startGame); startGame = gameUpToDate
  }
  def B5(): Unit = {
    val gameUpToDate = updateGame(btn_5,startGame); startGame = gameUpToDate
  }
  def B6(): Unit = {
    val gameUpToDate = updateGame(btn_6,startGame); startGame = gameUpToDate
  }
  def B7(): Unit = {
    val gameUpToDate = updateGame(btn_7,startGame); startGame = gameUpToDate
  }
  def B8(): Unit = {
    val gameUpToDate = updateGame(btn_8,startGame); startGame = gameUpToDate
  }

}


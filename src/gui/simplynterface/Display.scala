package gui.simplynterface

import data._
import representation._
import scala.swing.SimpleSwingApplication
import scala.swing.MainFrame
import scala.swing.Button
import scala.swing.TextField
import java.awt.Rectangle
import scala.swing.FlowPanel
import scala.swing.BorderPanel
import scala.swing.Panel
import scala.swing.TextArea
import java.awt.Dimension
import log._
import java.awt.Font
import scala.swing.ScrollPane
import java.awt.Color

abstract class Display {
  def printString(str: String): Unit
  def printNewLine(): Unit
  def printAppend(str: String): Unit
  def clear(): Unit
  def printMap(map: LandInterface, player: SelfInterface, objects: List[ObjectInterface]): Unit
  def printDesc(elm: Representation): Unit
  def printActions(actions: List[((Int, Int), ActionInterface)], inContainer: Boolean): Unit
  def printInventory(inventory: List[(Int, ContentInterface)]): Unit
  def quit(): Unit
}

class RealDisplay extends Display {
  def printString(str: String) {
    println(str);
  }

  def printNewLine() {
    println()
  }

  def printAppend(str: String) {
    print(str)
  }

  def clear() {}

  def printMap(map: LandInterface, player: SelfInterface, objects: List[ObjectInterface]) { //TODO with representation
    var playerX: Int = -1
    var playerY: Int = -1
    player.position match {
      case Coordinates(x, y, _) => playerX = x; playerY = y
      case _ => playerX = (map.width - 1) / 2; playerY = (map.height - 1) / 2
    }

    val maxView: Int = 5
    val maxY = (playerY + maxView)
    val minY = (playerY - maxView)
    val maxX = (playerX + maxView)
    val minX = (playerX - maxView)

    this.printString("-------------------------------------Etage " + map.floor.z + "---------------------------------")
    for (y <- maxY until minY by -1) {
      for (x <- minX until maxX) {
        // Calcul des coordonnées relative
        if (y == playerY && x == playerX) {
          this.printPlayer(player.identifier) // TODO MIEUX!
        } else if (!this.printObjects(x, y, objects)) {
          this.printTile(map.tiles.find(elm => elm._2.position match { case Coordinates(ex, ey, _) => (ex == x && ey == y) case _ => false }))
        }
      }
      this.printNewLine()
    }
    this.printString("-------------------------------------Etage " + map.floor.z + "---------------------------------")
  }

  private def printTile(tile: Option[(Int, TileInterface)]) {
    var id = Identifier.empty
    tile match {
      case None => ()
      case Some(t) => id = t._2.identifier
    }

    val str = id match {
      case Identifier.empty => "⚅"
      case Identifier.ground => "⚀" //_
      case Identifier.wall => "⚅"
      case _ => "∅"
    }
    this.printAppend(" " + str)
  }

  private def printPlayer() {
    this.printAppend(" ☃") //☺
  }

  private def printPlayer(id: Int) {
    val str = id match {
      case Identifier.doorClosed => "☗"
      case Identifier.doorOpen => "☖"
      case Identifier.goblin => "☹"
      case Identifier.snake => "§"
      case Identifier.fountain => "⨀"
      case Identifier.flask => "∐"
      case _ => "☂"
    }
    this.printAppend(str)
  }

  def printDesc(elm: Representation) {
    this.printString("---------------------------------------")
    this.printString(elm.description)
    this.printString("---------------------------------------")
  }

  def printActions(actions: List[((Int, Int), ActionInterface)], inContainer: Boolean) {
    this.printNewLine()
    if (actions.size <= 0) {
      this.printString("Pas d'action possible.")
    } else {
      this.printString(actions.size + " actions possibles :")
      var idx = 1
      if (!inContainer) {
        // Actions du joueur
        actions foreach {
          elm =>
            if (elm._1._2 == -1) {
              this.printString(idx + " " + elm._2.name + " : " + elm._2.description + " ( " + elm._2.time + " tours )")
              idx += 1
            }
        }

        this.printNewLine();
        this.printString("Actions à proximité :")
      }

      // Actions sur les objets
      actions foreach {
        elm =>
          if (elm._1._2 >= 0) {
            this.printString(idx + " " + elm._2.name + " : " + elm._2.description + " ( " + elm._2.time + " tours )")
            idx += 1
          }
      }
    }
  }

  def printObjects(cx: Int, cy: Int, objects: List[ObjectInterface]): Boolean = {
    val oi = objects.find(elm => elm.position match { case Coordinates(x, y, _) => (x == cx && y == cy) case _ => false })
    oi match {
      case None => false
      case Some(o) => this.printObject(o); true
    }
  }

  private def printObject(o: ObjectInterface) {
    val str = o.identifier match {
      case Identifier.doorClosed => "☗"
      case Identifier.doorOpen => "☖"
      case Identifier.goblin => "☹"
      case Identifier.snake => "§"
      case Identifier.fountain => "⨀"
      case Identifier.flask => "∐"
      case _ => "☂"
    }
    this.printAppend(" " + str)
  }

  def printInventory(inventory: List[(Int, ContentInterface)]) {
    this.printString(inventory.size + " objets dans votre inventaire :")
    inventory foreach {
      elm =>
        this.printString(elm._2.name + " : " + elm._2.description)
    }
  }

  def quit() {}
}

class SwingDisplay(frame: FrameDisplay) extends RealDisplay {
  override def printString(str: String) {
    this.frame.display.append(str + sys.props("line.separator").toString())
  }

  override def printNewLine() {
    this.frame.display.append(sys.props("line.separator").toString())
  }

  override def printAppend(str: String) {
    this.frame.display.append(str)
  }

  override def clear() {
    this.frame.display.text = ""
  }

  override def quit() {
    this.frame.quit()
  }
}

class FrameDisplay extends SimpleSwingApplication {
  val display = new TextArea()
  val scroll = new ScrollPane(display) //{ add(display, BorderPanel.Position.North) }
  val entry = new TextField()
  val mappanel = new FlowPanel()
  val panel = new BorderPanel() {
    add(mappanel, BorderPanel.Position.North)
    add(scroll, BorderPanel.Position.Center)
    add(entry, BorderPanel.Position.South)
  }

  display.editable = false
  display.focusable = false
  display.lineWrap = true
  display.wordWrap = true
  display.font = new Font(Font.SANS_SERIF, Font.PLAIN, 16)

  this.startup(null)

  def top = new MainFrame {
    title = "SimplYnterface v1.8.3 - <Insert Name Here>"
    contents = panel
    size = new Dimension(900, 800)
  }
}

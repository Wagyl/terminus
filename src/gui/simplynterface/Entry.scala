package gui.simplynterface

import scala.swing.TextField
import scala.swing.Reactor
import scala.swing.event.KeyPressed
import scala.swing.event.Key
import representation.Identifier

abstract class Entry(var shortcut: Boolean) {
  def getRespons(): String
}

class RealEntry extends Entry(false) {
  def getRespons() = {
    val str: String = readLine()
    if (str == null) {
      ""
    } else {
      str.toLowerCase()
    }
  }
}

class SwingEntry(entry: TextField) extends Entry(true) with Reactor {
  var pressed = false
  var respons: String = null;
  this.listenTo(entry.keys)
  this.reactions += {
    case KeyPressed(_, Key.Enter, _, _) => this.pressed = true
    case KeyPressed(_, Key.Up, _, _) => if (this.shortcut) { this.respons = "/sh" + Identifier.moveTop; this.pressed = true }
    case KeyPressed(_, Key.Down, _, _) => if (this.shortcut) { this.respons = "/sh" + Identifier.moveBottom; this.pressed = true }
    case KeyPressed(_, Key.Left, _, _) => if (this.shortcut) { this.respons = "/sh" + Identifier.moveLeft; this.pressed = true }
    case KeyPressed(_, Key.Right, _, _) => if (this.shortcut) { this.respons = "/sh" + Identifier.moveRight; this.pressed = true }
  }

  def getRespons() = {
    respons = null
    this.pressed = false
    while (!pressed) {
      Thread.sleep(100)
    }
    this.pressed = false
    val str = entry.text.toLowerCase()
    entry.text = ""
    if (respons != null) {
      respons
    } else {
      str
    }
  }
}
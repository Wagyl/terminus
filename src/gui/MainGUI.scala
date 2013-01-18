package gui

import gui.simplynterface.MainWindow
import communication.ConnectionsManager
import representation.Identifier

object MainGui {
  def main(args: Array[String]) {
    val parsedArgs = ArgsParser.parse(args :+ "-nointro")

    if (parsedArgs.contains(Identifier.server)) {
      val server = new Thread(new ConnectionsManager(8642, parsedArgs))
      server.start()
      println("Serveur lancé")
    } else {
      val m = new MainWindow()
      m.start(parsedArgs)
    }
  }
}

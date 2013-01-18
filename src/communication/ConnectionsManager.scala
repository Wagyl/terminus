package communication

import controller._
import representation._
import controller._

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

/*
 *
 */

class ConnectionsManager(val port: Int, val extensionsIds: List[Int]) extends Runnable {
  private var controller: In = null
  private var status = true
  private val server = new ServerSocket(port)

  def run(): Unit = {
    try {
      while (status) {
        val sk: Socket = server.accept()

        val connection: ServerConnection = new ServerConnection(sk)

        val client = new DistantClient(connection)

        /* Créer une nouvelle partie pour le joueur entrant */
        /* Créer un nouveau controleur de jeu */
        this.controller = InitControl(client).createGame(0, 0, extensionsIds)

        connection.controller = this.controller
        /*
         * Ici, il serait bon de prévenir le controller de la connexion...
         */
        (new Thread(connection)).start()
      }
    } catch {
      case e ⇒ e.printStackTrace()
    }
  }
}
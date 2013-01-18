package communication

import communication.messages._
import communication.parser._
import controller._

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.IOException
import java.io.PushbackInputStream
import java.net.Socket
import java.net.SocketTimeoutException

sealed abstract class Connection (sk : Socket) extends Runnable {

  protected val socket : Socket = sk
  socket.setSoTimeout(3000)

  private val in : PushbackInputStream = new PushbackInputStream(
    new BufferedInputStream(socket.getInputStream()))

  private val out : BufferedOutputStream = new BufferedOutputStream(
    socket.getOutputStream())


  def process (message : Message) : Unit


  def send (message : Message) : Unit = {
    // println("SEND : " +message.toString)
    message.format(out)
    out.flush()
  }

  def run () : Unit = {
    try {
      while (true) {
	try {
	  val message = Message.parse(in)
	  // println("RECEIVE : " +message.toString)
	  process(message)
	} catch {
	  case e : SocketTimeoutException =>
	}
      }
    } catch {
      case ProtocolParseError(e) => println(e)
      case ProtocolError(e) => println(e)
      case x =>
    }
    try {
      close();
    } catch { case _ => {} }
  }


  def close () : Unit = {
    println("CONNECTION CLOSED")
    if (!this.socket.isClosed()) {
      this.in.close()
      this.out.close
      this.socket.close()
    }
  }
}


/*
 * Connexion concrète coté serveur.
 */

class ServerConnection (sk : Socket) extends Connection (sk) {

  final var controller : In = null

  final def process (message : Message) = message match {
    case m : ClientMessage => m.process(controller, this)
    case m : ServerMessage => throw ProtocolError("")
    case _ => throw ProtocolError("")
  }

}

/*
 * Connexion concrète coté client.
 */

class ClientConnection (host : String, port : Int,
  val gui : Out, val pending : Pending) extends Connection (new Socket(host, port)) {
  
  final def process (message : Message) = message match {
    case m : ServerMessage => m.process(gui, pending, this)
    case m : ClientMessage => throw ProtocolError("")
    case _ => throw ProtocolError("")
  }
}

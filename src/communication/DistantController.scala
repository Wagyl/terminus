package communication

import controller._
import communication.messages._
import representation._


class DistantController (host : String, port : Int, client : Out) 
extends GameControl with Pending {

  val connection : Connection = new ClientConnection (host, port, client, this)
  (new Thread(connection)).start()

  private var _nonce : Int = 0

  private def nonce () : Int = {
    val n = _nonce;
    _nonce += 1
    n
  }

  override def make (action : Int) : Result = {
    val n = nonce
    val message = MakeMessage(n, action)
    connection.send(message)
    val result = wait(n)
    result
  }

  override def use (entity : Int, use : Int) : Result = {
    val n = nonce
    val message = UseMessage(n, entity, use)
    connection.send(message)
    val result = wait(n)
    result
  }

  override def consume (entity : Int, edible : Int) : Result = {
    val n = nonce
    val message = ConsumeMessage(n, entity, edible)
    connection.send(message)
    val result = wait(n)
    result
  }

  override def save () : Unit = { }
  
  override def quit (save : Boolean) : Unit = { }
}
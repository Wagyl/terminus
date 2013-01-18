package communication

import controller.Out
import controller.In
import representation._
import communication.messages._

import java.net.Socket

/*
 *
 */

class DistantClient (connection : Connection) extends Out {

  val client : Connection = connection
  
  override def character (data : (Int, SelfInterface)) : Unit = {
    val message = SelfMessage(data)
    client.send(message)
  }

  override def objects (data : List[(Int, EntityInterface)]) : Unit = {
    val message = ObjectMessage(data)
    client.send(message)
  }

  override def inventory (data : List[(Int, ContentInterface)]) : Unit = {
    val message = InventoryMessage(data)
    client.send(message)
  }

  override def maps (data : LandInterface) : Unit = {
    val message = MapMessage(data)
    client.send(message)
  }

  override def allObjects(data: List[((Int, TileInterface), List[(Int, EntityInterface)])]): Unit = {}
  
  def event(data: EventInterface): Unit = {}
}

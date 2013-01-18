package communication.messages

import communication.parser._
import communication._
import controller.Out
import representation._

/*
 * Messages du protocole permettant le transfert des données du jeu
 * du serveur au client.
 */

/*
 * Correspond à la méthode character de Out.
 */
class SelfMessage (content : List[LexUnit]) extends ServerMessage (content) {
  override def process (out : Out, pending : Pending, server : Connection) = {
    val id = nth(1).getIntValue
    val lexself = nth(2)
    val self : SelfInterface = SelfInterface.extract(lexself)
    out.character(id, self)
  }
}

object SelfMessage {
  def apply (data : (Int, SelfInterface)) : SelfMessage = {
    val content = List[LexUnit](LexInt(Protocol.self), LexInt(data._1),
      data._2.lexem)
    new SelfMessage(content)
  }
}


/*
 * Correspond à la méthode objects de Out.
 */
class DataMessage (content : List[LexUnit]) extends ServerMessage (content) {
  override def process (out : Out, pending : Pending, server : Connection) = {
    val lexdata = nth(1)
    val data = EntityInterface.extractList(lexdata)
    out.objects(data)
  }
}

object ObjectMessage {
  def apply (objects : List[(Int, EntityInterface)]) : DataMessage = {
    val lexlist = EntityInterface.lexem(objects)
    val content = List[LexUnit](LexInt(Protocol.data), lexlist)
    new DataMessage(content)
  }
}


/*
 * Correspond à la méthode inventory de Out.
 */
class InventoryMessage (content : List[LexUnit]) extends ServerMessage (content) {
  override def process (out : Out, pending : Pending, server : Connection) = {
    val lexdata = nth(1)
    val list = EntityInterface.extractList(lexdata)
    val contents = list.filter(_._2.isInstanceOf[ContentInterface])
    out.inventory(contents.map(_.asInstanceOf[(Int, ContentInterface)]))
  }
}

object InventoryMessage {
  def apply (objects : List[(Int, ContentInterface)]) : InventoryMessage = {
    val lexlist = EntityInterface.lexem(objects)
    val content = List[LexUnit](LexInt(Protocol.data), lexlist)
    new InventoryMessage(content)
  }
}

/*
 * Correspond à la méthode maps de Out.
 */
class MapMessage (content : List[LexUnit]) extends ServerMessage (content) {
  override def process (out : Out, pending : Pending, server : Connection) = {
    val lexland = nth(1) 
    val land = LandInterface.extract(lexland)
    out.maps(land)
  }
}

object MapMessage {
  def apply (map : LandInterface) : MapMessage = {
    val lexland = map.lexem
    val content = List[LexUnit](LexInt(Protocol.map), lexland)
    new MapMessage(content)
  }
}

package representation

import data._
import communication.parser._

/*
 * Représentation des Évènements.
 */
abstract class EventInterface (name : String, identifier : Int)
	 extends Representation (name, identifier) {
  val reason : Int
  val data : String
  var description = ""

  def lexname () = "EVENT"
  def lexcontent () : List[LexUnit] = {
    List(LexString(name), LexInt(identifier), LexInt(reason),
      LexString(data))
  }
}

object EventInterface extends StreamCompanion[EventInterface] {
  def lexname () = "EVENT"
  
  def extract (content : List[LexUnit]) : EventInterface = {
    if (content.length != 4)
      throw ProtocolError("Ceci n'est pas un Event : " +content)
    val name = content(0).getStringValue
    val id = content(1).getIntValue
    val _reason = content(2).getIntValue
    val _data = content(3).getStringValue
    new EventInterface(name, id) {
      val reason = _reason
      val data = _data
    }
  }
}

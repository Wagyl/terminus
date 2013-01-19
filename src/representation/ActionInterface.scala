package representation

import communication.parser._

/*
 * Représentation des Actions.
 */

abstract class ActionInterface (name : String, identifier : Int) 
extends Representation (name, identifier) with Stream {

  /*
   * Attention, ce temps n'est qu'une estimation. Il ne reflète pas forcément
   * le temps qui sera réellement pris par son exécution.
   */
  var time : Double
  
  def lexname () = "ACTION"

  def lexcontent () : List[LexUnit] = {
    val t = (time * 100).toInt
    List(LexString(name), LexInt(identifier), 
      LexString(description), LexInt(t))
  }	   
}


object ActionInterface extends StreamCompanion[ActionInterface] {
  def lexname () = "ACTION"

  def extract (content : List[LexUnit]) : ActionInterface = {
      if (content.length != 4)
	throw ProtocolError("Action mal formée : " +content)
      val name = content(0).getStringValue
      val identifier = content(1).getIntValue
      val desc = content(2).getStringValue
      val t = (content(3).getIntValue).toDouble / 100
      new ActionInterface(name, identifier) { 
	var description = desc
	var time = t 
      }
  }
}

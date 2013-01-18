package representation

import communication.parser._

/*
 * Représentation des Actions.
 */

abstract class ActionInterface (name : String, identifier : Int) 
extends Representation (name, identifier) {

  /*
   * Attention, ce temps n'est qu'une estimation. Il ne reflète pas forcément
   * le temps qui sera réellement pris par son exécution.
   */
  var time : Double
  
  def lexem () : LexObject = { 
    val lexname = LexAtom("ACTION")
    val t = (time * 100).toInt
    val content = List(LexString(name), LexInt(identifier), 
      LexString(description), LexInt(t))
    new LexObject(lexname, content)
  }	   
}


object ActionInterface {
  def extract (lexem : LexUnit) : ActionInterface = lexem match {
    case lexobj : LexObject =>
    if (lexobj.getName != "ACTION")
      throw ProtocolError ("Ceci ne représente pas une Action : " +lexem)
    else {
      val content = lexobj.getContent
      if (content.length != 4)
	throw ProtocolError("Action mal formée : " +lexem)
      val name = content(0).getStringValue
      val identifier = content(1).getIntValue
      val desc = content(2).getStringValue
      val t = (content(3).getIntValue).toDouble / 100
      new ActionInterface(name, identifier) { 
	var description = desc
	var time = t 
      }
    }

    case _ => throw ProtocolError ("Ceci n'est pas un objet : " +lexem)

  }

  def lexem (list : List[(Int, ActionInterface)]) : LexUnit = {
    val lexlist = list.map(x => LexPair(LexInt(x._1), x._2.lexem))
    LexList(lexlist)
  }

  def extractList (lexem : LexUnit) : List[(Int, ActionInterface)] = {
    val list = lexem.getListValue
    val lexpairs = list.map(lex => (lex.getFirst, lex.getSecond))
    lexpairs.map(x => (x._1.getIntValue, ActionInterface.extract(x._2)))
  }
}

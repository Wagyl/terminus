package representation

import data._
import communication.parser._

/*
 * Représentation des Cartes.
 */
abstract class LandInterface (name : String, identifier : Int) 
	 extends Representation (name, identifier) {
  	   val height : Int
  	   val width : Int
  	   val floor : Floor
	   var tiles : List[(Int, TileInterface)] = List()

	   def lexem () : LexObject = {
	     val lexname = LexAtom("LAND")
	     val lexlist = EntityInterface.lexem(tiles)
	     val content = List( LexString(name), LexInt(identifier),
	       LexString(description), LexInt(height), LexInt(width),
	       LexInt(floor.z), lexlist)
	     new LexObject(lexname, content)
	   }
	 }

object LandInterface {

  def extract (lexem : LexUnit) : LandInterface = lexem match {
    case lexobj : LexObject =>
    if (lexobj.getName != "LAND")
      throw ProtocolError ("Ceci ne représente pas un Land : " +lexem)
    else {
      val content = lexobj.getContent
      if (content.length != 7)
	throw ProtocolError("Land mal formé : " +lexem)
      val name = content(0).getStringValue
      val identifier = content(1).getIntValue
      val _description = content(2).getStringValue
      val _height = content(3).getIntValue
      val _width = content(4).getIntValue
      val _floor = Floor(content(5).getIntValue)
      val list = EntityInterface.extractList(content(6))
      val tlist = list.filter(_._2.isInstanceOf[TileInterface])
      val _tiles = tlist.map(x => (x._1, x._2.asInstanceOf[TileInterface]))

      new LandInterface (name, identifier) {
	var description = _description
	val height = _height
  	val width = _width
  	val floor = _floor
	tiles = _tiles
      }
    }
  
    case _ => throw ProtocolError ("Ceci n'est pas un objet : " +lexem)
  }

}

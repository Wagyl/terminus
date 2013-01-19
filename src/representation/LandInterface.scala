package representation

import data._
import communication.parser._

/*
 * Représentation des Cartes.
 */
abstract class LandInterface (name : String, identifier : Int) 
	 extends Representation (name, identifier) with Stream {
  	   val height : Int
  	   val width : Int
  	   val floor : Floor
	   var tiles : List[(Int, TileInterface)] = List()

	   def lexname () = "LAND"

	   def lexcontent () : List[LexUnit] = {
	     val lexlist = Stream.lexeme(tiles)
	     List( LexString(name), LexInt(identifier),
	       LexString(description), LexInt(height), LexInt(width),
	       LexInt(floor.z), lexlist)
	   }
	 }

object LandInterface extends StreamCompanion[LandInterface] {
  def lexname () = "LAND"

  def extract (content : List[LexUnit]) : LandInterface = {
      if (content.length != 7)
	throw ProtocolError("Land mal formé : " +content)
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

}

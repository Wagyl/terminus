package representation

import data._
import objects.Entity
import communication.parser._

abstract class Representation (val name : String, val identifier : Int) {
  /* Courte description à destination du joueur. */
  var description : String
}


object Representation {

  /*
   * Si [elements] est une liste d'objets représentables, i.e. possédant
   * une méthode representation : Entity => Option[T] avec T <: Representation,
   * cette méthode renvoie la liste de couples (index, T) telle que :
   * - les résultats None ont été "effacés"
   * - les index correspondent aux index des objets représentés dans la liste
   * initiale (les index des None ne sont donc pas présents)
   */
  def get[T] (observer : Entity,
    elements : List[ {def representation (observer : Entity) : Option[T]} ])
  : List[(Int, T)] = 
    {
      val optionsRep = elements.map(_.representation(observer))
      val optionsRepIndex = optionsRep.zipWithIndex.map(_.swap)
      val repIndexOptions = optionsRepIndex.map(expand)
      val repIndex = repIndexOptions.flatMap(x => x)
      repIndex
    }

  def expand[A,B] (c : (A,Option[B])) : Option[(A, B)] = c match {
    case (x, Some(y)) => Some((x, y))
    case (_, None) => None
  }

  /*
   *
   */
  def round (value : Double, precision : Int) : Double = { 
    val s = math pow (10, precision)
    (math round value * s) / s
  }
}




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




/*
 * Représentation des Évènements.
 */
abstract class EventInterface (name : String, identifier : Int) 
	 extends Representation (name, identifier) {
	val reason : Int
	val data : String
	var description = ""
}

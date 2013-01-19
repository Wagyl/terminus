package representation

import data._
import communication.parser._

/*
 * Représentation des données pour l'interface utilisateur.
 */
sealed abstract class EntityInterface (name : String, identifier : Int)
		extends Representation (name, identifier) {
  
  /* Extensions éventuellements ajoutées par des traits des Entity. */
  var extensions : List[Extension] = List()
  
  def lexname () = "ENTITY"
}


/*
 * Représentations des objets Able, Edible et Usable...
 */

trait OnMap { val position : Coordinates }
trait AbleInterface { var abilities : List[(Int, ActionInterface)] = List() }
trait EdibleInterface { var usages : List[(Int, ActionInterface)] = List() }
trait UsableInterface { var uses : List[(Int, ActionInterface)] = List() }


/*
 * Objet compagnon...
 */
object EntityInterface extends StreamCompanion[EntityInterface] {
  def lexname () = "ENTITY"

  def extract (content : List[LexUnit]) : EntityInterface = {
    if (content.length < 1)
      throw ProtocolError("Entity mal formé : " +content)
    val code = content(0).getIntValue
    val name = content(1).getStringValue
    val identifier = content(2).getIntValue
    val _description = content(3).getStringValue
    code match {
      /* SelfInterface */
      case 0 => 
      val _position = Coordinates.extract(content(4))
      val _abilities = ActionInterface.extractList(content(5))
      new SelfInterface(name, identifier) {
	var description = _description
	val position = _position
	abilities = _abilities
      }
      /* ContentInterface */
      case 1 =>
      val _usages = ActionInterface.extractList(content(4))
      new ContentInterface(name, identifier) {
	var description = _description
	usages = _usages
      }
      
      /* ObjectInterface */
      case 2 =>
      val _position = Coordinates.extract(content(4))
      val _uses = ActionInterface.extractList(content(5))
      new ObjectInterface(name, identifier) {
	var description = _description
	val position = _position
	uses = _uses
      }
      
      /* TileInterface */
      case 3 =>
      val _position = Coordinates.extract(content(4))
      new TileInterface(name, identifier) {
	var description = _description
	val position = _position
      }

      case _ => throw ProtocolError("Entity mal formé : " +content)
    }
  }
}




// S'il s'agit du joueur lui-même :
abstract class SelfInterface (name : String, identifier : Int) 
	 extends EntityInterface (name, identifier) 
	 with OnMap with AbleInterface {

	   override def lexcontent () : List[LexUnit] = {
	     List(LexInt(0), LexString(name),
	       LexInt(identifier), LexString(description),
	       position.lexeme, Stream.lexeme(abilities))
	   }
	   
	 }

object SelfInterface extends StreamCompanion[SelfInterface] {

  def lexname = "ENTITY"

  def extract (content : List[LexUnit]) : SelfInterface = {
    if (content(0).getIntValue != 0)
      throw ProtocolError("Ceci n'est pas un SelfInterface : " +content)
    val name = content(1).getStringValue
    val identifier = content(2).getIntValue
    val _description = content(3).getStringValue
    val _position = Coordinates.extract(content(4))
    val _abilities = ActionInterface.extractList(content(5))
    new SelfInterface(name, identifier) {
      var description = _description
      val position = _position
      abilities = _abilities
    }
  }

}


// S'il s'agit d'un objet que possède le joueur :
abstract class ContentInterface (name : String, identifier : Int)
	 extends EntityInterface (name, identifier)
	 with EdibleInterface { 

	   override def lexcontent () : List[LexUnit] = {
	     List(LexInt(1), LexString(name),
	       LexInt(identifier), LexString(description),
	       Stream.lexeme(usages))
	   }
	 }

// S'il s'agit d'un objet de la carte (objet, crétature, parcelle...) :
abstract class ObjectInterface (name : String, identifier : Int) 
	 extends EntityInterface (name, identifier) 
	 with OnMap with UsableInterface { 
	 
	   override def lexcontent () : List[LexUnit] = {
	     List(LexInt(2), LexString(name),
	       LexInt(identifier), LexString(description),
	       position.lexeme, Stream.lexeme(uses))
	   }
	 }

// S'il s'agit d'un objet de décors de la carte (Tile,...) :
abstract class TileInterface (name : String, identifier : Int) 
	 extends EntityInterface (name, identifier)
	 with OnMap { 

	   override def lexcontent () : List[LexUnit] = {
	     List(LexInt(3), LexString(name),
	       LexInt(identifier), LexString(description),
	       position.lexeme)
	   }
	 }





/*
 * Extensions des objets éventuellement définies par de nouveaux traits
 * des Entity.
 */
abstract class Extension { }

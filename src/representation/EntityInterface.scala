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

		  def lexem () : LexObject
		  
		}


object EntityInterface {

  def extract (lexem : LexUnit) : EntityInterface = lexem match {
    case lexobj : LexObject =>
      if (lexobj.getName != "ENTITY")
	throw ProtocolError ("Ceci ne représente pas une Entity : " +lexem)
      else {
	val content = lexobj.getContent
	if (content.length < 1)
	  throw ProtocolError("Entity mal formé : " +lexem)
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

	  case _ => throw ProtocolError("Entity mal formé : " +lexem)
	}
      }
    case _ => throw ProtocolError ("Ceci n'est pas un objet : " +lexem)
  }

  def lexem (list : List[(Int, EntityInterface)]) : LexUnit = {
    val lexlist = list.map(x => LexPair(LexInt(x._1), x._2.lexem))
    LexList(lexlist)
  }

  def extractList (lexem : LexUnit) : List[(Int, EntityInterface)] = {
    val list = lexem.getListValue
    val lexpairs = list.map(lex => (lex.getFirst, lex.getSecond))
    lexpairs.map(x => (x._1.getIntValue, EntityInterface.extract(x._2)))
  }
}


/*
 * Représentations des objets Able, Edible et Usable...
 */

trait OnMap { val position : Coordinates }
trait AbleInterface { var abilities : List[(Int, ActionInterface)] = List() }
trait EdibleInterface { var usages : List[(Int, ActionInterface)] = List() }
trait UsableInterface { var uses : List[(Int, ActionInterface)] = List() }


// S'il s'agit du joueur lui-même :
abstract class SelfInterface (name : String, identifier : Int) 
	 extends EntityInterface (name, identifier) 
	 with OnMap with AbleInterface {

	   
	   override def lexem () : LexObject = {
	     val lexname = LexAtom("ENTITY")
	     val content = List(LexInt(0), LexString(name),
	       LexInt(identifier), LexString(description),
	       position.lexem, ActionInterface.lexem(abilities))
	     new LexObject(lexname, content)
	   }
	   
	 }

object SelfInterface {

  def extract (lexem : LexUnit) : SelfInterface = lexem match {
    case lexobj : LexObject =>
      if (lexobj.getName != "ENTITY")
	throw ProtocolError ("Ceci ne représente pas une Entity : " +lexem)
      else {
	val content = lexobj.getContent
	if (content.length < 1)
	  throw ProtocolError("Entity mal formé : " +lexem)
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

	  case _ => throw ProtocolError("SelfEntity mal formé : " +lexem)
	}
      }
    case _ => throw ProtocolError ("Ceci n'est pas un objet : " +lexem)
  }
}


// S'il s'agit d'un objet que possède le joueur :
abstract class ContentInterface (name : String, identifier : Int)
	 extends EntityInterface (name, identifier)
	 with EdibleInterface { 

	   override def lexem () : LexObject = {
	     val lexname = LexAtom("ENTITY")
	     val content = List(LexInt(1), LexString(name),
	       LexInt(identifier), LexString(description),
	       ActionInterface.lexem(usages))
	     new LexObject(lexname, content)
	   }

}

// S'il s'agit d'un objet de la carte (objet, crétature, parcelle...) :
abstract class ObjectInterface (name : String, identifier : Int) 
	 extends EntityInterface (name, identifier) 
	 with OnMap with UsableInterface { 
	 
	 override def lexem () : LexObject = {
	     val lexname = LexAtom("ENTITY")
	     val content = List(LexInt(2), LexString(name),
	       LexInt(identifier), LexString(description),
	       position.lexem, ActionInterface.lexem(uses))
	     new LexObject(lexname, content)
	   }
	   
	 }

// S'il s'agit d'un objet de décors de la carte (Tile,...) :
abstract class TileInterface (name : String, identifier : Int) 
	 extends EntityInterface (name, identifier)
	 with OnMap { 

	   override def lexem () : LexObject = {
	     val lexname = LexAtom("ENTITY")
	     val content = List(LexInt(3), LexString(name),
	       LexInt(identifier), LexString(description),
	       position.lexem)
	     new LexObject(lexname, content)
	   }
	   
	 }





/*
 * Extensions des objets éventuellement définies par de nouveaux traits
 * des Entity.
 */
abstract class Extension { }

package data

import objects._
import representation._
import communication.parser._

/*
 * cette classe représente la position d'une Entity.
 */
abstract sealed class Location {
  def toString : String
}

/*
 * Sur une carte donnée.
 */
case class Floor (z:Int) extends Location { 
  override def toString : String = "Floor " + z
}

/*
 * Une position précise d'une carte.
 */
case class Coordinates (x:Int, y:Int, z:Floor) extends Location { 
  override def toString : String = "Coordinates : (" + x + ", " + y + ")"

  def lexem () : LexUnit = {
    val lexname = new LexAtom("COORDINATES")
    val content = List(LexInt(x), LexInt(y), LexInt(z.z))
    new LexObject(lexname, content)
  }
}

object Coordinates {
  def extract (lexem : LexUnit) : Coordinates = lexem match {
    case lexobj : LexObject =>
    if (lexobj.getName != "COORDINATES")
      throw ProtocolError ("Ceci ne représente pas une coordonnée : " +lexem)
    else {
      val content = lexobj.getContent
      if (content.length != 3)
	throw ProtocolError("Action mal formée : " +lexem)
      val x = content(0).getIntValue
      val y = content(1).getIntValue
      val z = content(2).getIntValue
      Coordinates(x, y, Floor(z))
    }

    case _ => throw ProtocolError ("Ceci n'est pas un objet : " +lexem)
  }
}

/*
 * L'objet est à l'intérieur d'un container.
 */
case class InContainer (container : Entity with Container) extends Location { 
  override def toString : String = "In Container " + container.name
}

/*
 * L'objet est à l'intérieur d'un container,
 * location à destinatin de l'interface.
 */
case class InContainerInterface (container : EntityInterface) extends Location {
  override def toString : String = "In Container " + container.name
}

/*
 * Tant que l'objet n'a pas été placé sur une carte...
 */
case class Nowhere () extends Location { 
   override def toString : String = "Nowhere"
}


/*
 * Lorsqu'il a été retiré du jeu...
 */
case class Removed () extends Location {
  override def toString : String = "Removed"
}

/*
 * À priori, ne devrait jamais arriver...
 */
case class Undefined () extends Location { 
  override def toString : String = "Undefined"
}



object Location {
  def representation (position : Location, observer : Entity) : Location =
    position match {
      case InContainer(container) => 
      container.representation(observer) match {
	case Some(c) => InContainerInterface(c)
	case None => Undefined ()
      }
      case _ => position
    }
  }

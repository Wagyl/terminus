package data

import objects._
import scala.collection.immutable._
import changes._

/*
 * Classe concrète des données.
 * Implémentation minimale naïve.
 */

class NaiveData (game : Game) extends Data (game) {

  private var hashmap : HashMap[Coordinates, List[Entity]] = new HashMap()


  override def getEntitiesAt(c : Coordinates) : List[Entity] = {
    hashmap.get(c) match {
      case Some(l) => l
      case None => List()
    }
  }

  override def getMutableEntities(m : Floor) : List[MutableEntity] = {
    val tuples = hashmap.toList
    def filter (el : (Coordinates, List[Entity])) : Boolean = el._1 == m
    val tuplesAtFloor = tuples.filter(filter)
    
    val listsEntities = tuples.map(_._2)
    def append (l1 : List[Entity], l2 : List[Entity]) : List[Entity] = l1 ::: l2
    val entities : List[Entity] = listsEntities.foldLeft(List[Entity]())(append)

    def filterMutable (e : Entity) = e match { 
      case _:MutableEntity => true
      case _ => false
    }
    val mutableEntities = entities.filter(filterMutable)

    mutableEntities.asInstanceOf[List[MutableEntity]]
  }
  
  override def getConstEntitiesAt(c : Coordinates) : List[ConstEntity] = {
    getEntitiesAt(c).filter(_.isInstanceOf[ConstEntity]).map(_.asInstanceOf[ConstEntity])
  }




  override def addEntityAt(obj : Entity, pos : Location) =
    pos match {
      case pos:Coordinates =>
        obj.game = game
	val l = getEntitiesAt(pos)
	val nl = obj :: l
	hashmap = hashmap + ((pos, nl))
	obj.position = pos
	true
      case _ => false
  }

  override def removeEntity (obj : Entity) : Boolean = {
    if (obj == game.character) {
      false
    }
    else {
      obj.position match {
	case x:Coordinates => 
	val l = getEntitiesAt(x)
	val nl = l.filterNot( (e:Entity) => e == obj)
	hashmap = hashmap.+((x, nl))
	obj.position = Nowhere()
	true
	case _ => false
      }
    }
  }

  override def moveEntityAt(obj : Entity, pos : Location) : Boolean = {
    (obj.position, pos) match {
      case (c:Coordinates, pos:Coordinates) =>
	val l = getEntitiesAt(c)
	val nl = l.filterNot( (e:Entity) => e == obj)
	hashmap = hashmap.+((c, nl))
	val m = getEntitiesAt(pos)
	val nm = obj :: m
	hashmap = hashmap + ((pos, nm))
	obj.position = pos
	true
      case _ => false
    }
  }
}

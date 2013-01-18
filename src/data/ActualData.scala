package data

import objects._
import scala.collection.immutable._

/*
 * Implémentation de Data, à l'aide de deux Vector de Map.
 */

class ActualData (game : Game) extends Data (game) {

  /* ----------------------
   * Structures de données.
   * ---------------------- */

  type MapList[E] = Map[(Int, Int), List[E]]
  type Table[E] = IndexedSeq[MapList[E]]

  // Table des éléments constants
  private var constTable : Table[ConstEntity] = Vector.empty

  // Table des éléments mutables
  private var mutableTable : Table[MutableEntity] = Vector.empty


  /* ------------------------------
   * Fonctions utilitaires privées.
   * ------------------------------ */

  /*
   * Fonctions d'accès rapide aux éléments des tables par la coordonnée.
   */

  private def getMap[T, E] (map : Map[T, List[E]]) (key : T) : List[E] =
    map.get(key) match {
      case Some(l) => l
      case None => List()
    }

  private def getTable[E] (table : Table[E]) (c : Coordinates) : List[E] = {
    val index = c.z.z
    if (index < 0 || index >= table.length)
      List()
    else
      getMap(table(index))((c.x, c.y))
  }

  /*
   * Fonctions d'ajout rapide d'éléments dans les tables.
   */

  private def putMap[T, E] (map : Map[T, List[E]], key : T, value : E) 
  : Map[T, List[E]] =
    map.get(key) match {
      case None => map + ((key, List(value)))
      case Some(l) => map + ((key, value :: l))
    }

  private def putTable[E] (table : Table[E], key : Coordinates, value : E)
  : Table[E] = {
    val index = key.z.z
    if (index < 0)
      throw new IllegalFloor(key.z)
    else if (index >= table.length) {
      val t = table.padTo(index + 1, Map())
      t.updated(index, Map((key.x, key.y) -> List(value)))
    }
    else {
      val map = putMap(table(index), (key.x, key.y), value)
      table.updated(index, map)
    } 
  }

  /*
   * Fonctions de suppression.
   */

  private def removeMap[T, E] (map : Map[T, List[E]], key : T, value : E)
  : Map[T, List[E]] = {
    val list = getMap(map)(key).filterNot(x => x == value)
    map + ((key, list))
  }

  private def removeTable[E] (table : Table[E], key : Coordinates, value : E)
  : Table[E] = {
    val index = key.z.z
    if (index < 0 || index >= table.length)
      throw new IllegalFloor(key.z)
    else {
      val map = removeMap(table(index), (key.x, key.y), value)
      table.updated(index, map)
    }
  }

  
  /* -----------------------------------
   * Implémentation de l'interface DATA.
   * ----------------------------------- */


  /*
   * Fonctions d'accès aux Entity.
   */

  override def getConstEntitiesAt (c : Coordinates) = getTable (constTable) (c)
  
  override def getEntitiesAt (c : Coordinates) : List[Entity] = {
    val index = c.z.z
    val consts = getConstEntitiesAt(c)
    val mutables = getTable (mutableTable) (c)
    consts ++ mutables
  }


  override def getMutableEntities (land : Floor) : List[MutableEntity] = {
    val index = land.z
    if (index < 0 || index >= mutableTable.length)
      List()
    else
      mutableTable(index).toList.flatMap(_._2).diff(List(game.character))
  }


  /*
   * Ajouts d'Entity.
   */

  def addEntityAt(obj : ConstEntity, c : Coordinates) : Boolean = {
    try {
      constTable = putTable(constTable, c, obj)
      obj.game = this.game;
      obj.position = c
      true
    } catch {
      case _ => println("ERR CONST"); false
    }
  }

  def addEntityAt(obj : MutableEntity, c : Coordinates) : Boolean = {
    try {
      mutableTable = putTable(mutableTable, c, obj)
      obj.game = this.game;
      obj.position = c
      true
    } catch {
      case _ => println("ERR MUT"); false
    }
  }

  def AddEntityIn(content : Content, pos : InContainer) : Boolean = {
    val container = pos.container
    val result = container.addContent(content)
    if (result) {
      content.game = this.game;
      content.position = pos
    }
    result
  }

  override def addEntityAt (obj : Entity, pos : Location) : Boolean =
    (pos, obj) match {
      case (c : Coordinates, const : ConstEntity) => addEntityAt(const, c)
      case (c : Coordinates, mutable : MutableEntity) => addEntityAt(mutable, c)
      case (in : InContainer, content : Content) => AddEntityIn(content, in)
      case _ => println("..."); false
  }

  
  /*
   * Suppression d'Entity.
   */
  
  // Attention, il faut que c soit bien la position de obj.
  private def removeEntity (obj : ConstEntity, c : Coordinates) : Boolean =
    try {
      constTable = removeTable(constTable, c, obj)
      obj.position = Removed ()
      true
    } catch {
      case _ => false
    }

  // Attention, il faut que c soit bien la position de obj.
  private def removeEntity (obj : MutableEntity, c : Coordinates) : Boolean =
    try {
      mutableTable = removeTable(mutableTable, c, obj)
      obj.position = Removed ()
      true
    } catch {
      case _ => false
    }

  override def removeEntity (obj : Entity) : Boolean =
    (obj.position, obj) match {
      case (c : Coordinates, const : ConstEntity) => removeEntity(const, c)
      case (c : Coordinates, mutable : MutableEntity) => removeEntity(mutable, c)

      case (c : InContainer, content : Content) =>
      val rv = c.container.removeContent(content)
      if (rv) content.position = Removed ()
      rv

      case _ => false
  }


  /*
   * Déplacement d'Entity.
   */
  
  override def moveEntityAt (obj : Entity, pos : Location) : Boolean = {
    val oldPos = obj.position
    val rv = addEntityAt(obj, pos)
    val newPos = obj.position
    if (rv) {
      obj.position = oldPos
      removeEntity(obj)
      obj.position = newPos
    }
    rv
  }
}


case class IllegalFloor (f : Floor) extends Exception { }

package data

import scala.collection.immutable.Vector

/*
 * Représentation du monde.
 */

abstract class World (val game : Game) {

  def name : String

  def size : Int
  
  def land (floor : Floor) : Land

  def addLand (typ : LandType, name : String, desc : String) : Floor

  def setLand (land : Land, floor : Floor) : Unit

  /*
   * Renvoie le Tile d'une position donnée sur une carte donnée.
   */
  def getTileAt(c:Coordinates) : Tile = {
    land(c.z).getTile(c.x, c.y)
  }
}


/*
 * Attention, cette classe concrète ne gère aucune exception ou erreur.
 */
class RealWorld (game : Game) extends World (game) {
  
  val name : String = "World"

  private var lands : Vector[Land] = Vector.empty

  def size : Int = lands.size

  def land (floor : Floor) : Land = {
    lands(floor.z)
  }

  def addLand (typ : LandType, name : String, desc : String) : Floor = {
    val floor = Floor(lands.size)
    val land = Land(typ, floor, name, desc, game)
    lands = lands :+ land
    floor
  }

  def setLand (land : Land, floor : Floor) : Unit = { 
    lands.updated(floor.z, land)
  }
}

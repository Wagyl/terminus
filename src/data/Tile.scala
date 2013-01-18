package data

import objects.Entity
import representation.Identifier
import representation.EntityInterface
import representation.TileInterface
import representation.ObjectInterface
import representation.ContentInterface

/**
 * Classe de base pour toutes représentations d'une case sur la carte
 */
abstract sealed class Tile(val speed: Double) extends Entity {
  /*
   * La vitesse est la durée d'un déplacement sur la case.
   * Une valeur strictement négative signifie qu'un tel déplacement
   * est impossible.
   */

  // TODO gérer le cas de observer = this !?
  override def representation(observer: Entity): Option[EntityInterface] = {
    val r = super.representation(observer)
    val that = this

    // Si il est sur la carte
    that.position match {
      case Coordinates(_, _, _) =>
        val ti = new TileInterface(that.name, that.identifier) {
          val position = that.position.asInstanceOf[Coordinates]
          var description = that.description
        }
        Some(ti)
      case _ => super.representation(observer)
    }
  }
}

/**
 * Usine permettant d'instancier une classe concrête de Tile en fonction des données numériques de la carte
 *
 */
object TileFactory {
  def fromMap(id: Char) :Option[Tile] = {
    id match {
      case Identifier.transparent_wall => Some(TransparentWall())
      case Identifier.empty => Some(Empty())
      case Identifier.ground => Some(Ground())
      case Identifier.wall => Some(Wall())
      case Identifier.indeterminate => None
      case _ => Some(Empty())
    }
  }
  def toMap(t: Tile): Char = {
    t.identifier.toChar
  }
}

/**
 * Objet pour les zones vides
 *
 */
case class Empty() extends Tile(-1) {
  val name = "Empty"
  val description = ""
  val identifier = Identifier.empty

  override def isVisible = false
}

/**
 * Objet pour le sol de base
 */
case class Ground() extends Tile(1) {
  val name = "Ground"
  val description = ""
  val identifier = Identifier.ground

  override def isStackable = true
}

/**
 * Objet pour les murs de base
 *
 */
case class Wall() extends Tile(-1) {
  val name = "Wall"
  val description = ""
  val identifier = Identifier.wall

  override def isTransparent = false
}

case class TransparentWall() extends Tile(-1) {
  val name = "Wall"
  val description = ""
  val identifier = Identifier.transparent_wall
}

package changes

import objects._
import data._
import representation._

/*
 * Classe des déplacements "normaux" vers une case voisine.
 * Attention, cette implémentation donnera des résultats possiblement
 * incohérent pour toute autre forme de déplacement.
 */
sealed abstract class MoveAction (abs : Int, ord : Int) extends Action {
  val name = "Move"
  
  var actor : Option[Entity with Moving] = None
  var tile : Option[Tile] = None

  // vecteur du déplacement
  val x : Int = abs
  val y : Int = ord
  val distance : Double = scala.math.sqrt(x * x + y * y)

  addEffect( new Effect {
    val change = new MoveChange(distance) 
    def effector = actor.get
    def target = tile.get
  })
  

  override def assign (a : Entity) : (Boolean, String) = {
    a match {
      case x : Entity with Moving => actor = Some(x)
      case _ => return (false, "Vous ne pouvez pas vous déplacer.")
    }
    a.position match {
      case Coordinates(abs, ord, z) =>

      /* nouvelle position après déplacement... */
      val newPos = Coordinates(abs + x, ord + y, z)

      /* si la parcelle n'est pas traversable... */
      val t = actor.get.game.world.getTileAt(newPos)
      if (t.speed < 0)
	return (false, "Vous ne pouvez vous déplacer ici : la case n'est pas accessible")

      /* si un objet sur la position est non stackable... */
      val entities = actor.get.game.data.getEntitiesAt(newPos)
      if (entities.exists(e => !e.isStackable))
	return (false, "Vous ne pouvez vous déplacer ici : un autre objet vous en empêche.")

      /* toutes les conditions sont vérifiées : */
      tile = Some(t)

      /* si la position de l'actor n'est pas une coordonnée... */
      case _ => return (false, "Vous ne pouvez pas vous déplacer.")
    }

    return (true, "Vous avancez.")
  }

  override def abort () : Unit = {
    actor = None
    tile = None
  }

  override def time (a : Entity) : Double = {
    var rv : Double = 0
    if (assign(a)._1) {
      val tileSpeed : Double = tile.get.speed
      val actorSpeed : Double = actor.get.speed
      rv = distance * actorSpeed * tileSpeed
    }
    abort ()
    rv
  }

  /*
   * Si l'action n'est pas possible parce qu'une Entity non Tile
   * bloque le passage, renvoie cette entité.
   */
  def target (a : Entity) : Option[Entity] = 
    a.position match {
      case Coordinates(abs, ord, z) =>

      /* nouvelle position après déplacement... */
      val newPos = Coordinates(abs + x, ord + y, z)
      
      /* si un objet sur la position est non stackable... */
      val entities = a.game.data.getEntitiesAt(newPos)
      return entities.find(e => !e.isStackable)

      case _ => return None
  }
}


/*
 * Actions concrètes (déplacemnt d'une case) :
 */

class MoveOnTop  extends MoveAction (0, 1) {
  val identifier = Identifier.moveTop
  val description = "Move Top."
}

class MoveOnBottom extends MoveAction (0, -1) {
  val identifier = Identifier.moveBottom
  val description = "Move Bottom."
}

class MoveOnLeft extends MoveAction (-1, 0) {
  val identifier = Identifier.moveLeft
  val description = "Move Left."
}

class MoveOnRight extends MoveAction (1, 0) {
  val identifier = Identifier.moveRight
  val description = "Move Right."
}


class MoveOnTopLeft extends MoveAction (-1, 1) {
  val identifier = Identifier.moveTopLeft
  val description = "Move Top Left."
}

class MoveOnTopRight extends MoveAction (1, 1) {
  val identifier = Identifier.moveTopRight
  val description = "Move Top Right."
}

class MoveOnBottomLeft extends MoveAction (-1, -1) {
  val identifier = Identifier.moveBottomLeft
  val description = "Move Bottom Left."
}

class MoveOnBottomRight extends MoveAction (1, -1) {
  val identifier = Identifier.moveBottomRight
  val description = "Move Bottom Right."
}



object MoveAction {

  val onTop = new MoveOnTop
  val onBottom = new MoveOnBottom
  val onLeft = new MoveOnLeft
  val onRight = new MoveOnRight
  val onTopLeft = new MoveOnTopLeft
  val onTopRight = new MoveOnTopRight
  val onBottomLeft = new MoveOnBottomLeft
  val onBottomRight = new MoveOnBottomRight

  /*
   * Renvoie la liste des 8 déplacements naturels pour l'entité donnée.
   */
  def getNaturalMove(c : Entity with Moving) : List[MoveAction] = {
    List(onTop, onBottom, onLeft, onRight, 
      onTopLeft, onTopRight, onBottomLeft, onBottomRight)
  }

}

package changes

import objects._
import data._

/*
 * Attention, ce mouvement se fait sans vérification s'il
 * est appelé.
 * (C'est à l'Action de vérifier si le mouvement est effectivement valide.)
 */
class MoveChange (val distance : Double) extends Change {

  val name = "Move"

  type Effector = Entity with Moving
  type Target = Tile
  
  override def apply (effector : Effector, target : Target)
  : (Outcome, Double) = {
    val success = effector.game.data.moveEntityAt(effector, target.position)
    val time = distance * target.speed * effector.speed
    (Outcome(success, ""), time)
  }
}

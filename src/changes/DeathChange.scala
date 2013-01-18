package changes

import objects._
import data._
import representation.Identifier

class DeathChange extends Change {

  val name = "Death"
  
  type Effector = Entity
  type Target = Entity with Life

  override def apply (effector : Effector, target : Target)
  : (Outcome, Double) = {
    if (target.health <= 0) {
      val result = target.game.data.removeEntity(target)
      val message =
	if (result) {
	  target match {
	    case x : Memory => x.addNotice (Identifier.gameEnd, Identifier.playerDeath, "Vous êtes mort. Game Over.")
	    case _ => ;
	  }
	  "La créature " + target.name + " est morte."
	}
	else "La créature " + target.name + " n'a pas voulu mourir."
      (Outcome(result, message), 0)
    }
    else 
      (Outcome(false, ""), 0)
  }
}

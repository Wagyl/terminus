package changes

import objects._
import data._

class PickUpChange extends Change {

  val name = "PickUp"

  type Effector = Entity with Able with Container
  type Target = Entity with Pickable

  override def possible (effector : Effector, target : Target) = {
    super.possible(effector, target) && (effector.position == target.position)
  }
  
  override def apply (effector : Effector, target : Target)
  : (Outcome, Double) = {
    val success = target.game.data.moveEntityAt(target, InContainer(effector))
    (Outcome(success, ""), 0.5)
  }
}



class DropChange extends Change {
  
  val name = "PickUp"

  type Effector = Entity with Able with Container
  type Target = Entity with Pickable

  override def possible (effector : Effector, target : Target) = {
    super.possible(effector, target) && (Container.contains(effector, target))
  }
  
  override def apply (effector : Effector, target : Target)
  : (Outcome, Double) = {
    val position = effector.position
    position match {
      case c : Coordinates =>
      val success = target.game.data.moveEntityAt(target, c)
      (Outcome(success, ""), 0.5)
      case _ => (Outcome(false, "Vous n'êtes pas sur le terrain."), 0.0)
    }
  }
}

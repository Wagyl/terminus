package changes

import objects._

class OpenChange extends Change {
  val name = "open"

  type Effector = Entity with Humanoid
  type Target = Entity with Openable

  override def possible (effector : Effector, target : Target) = {
    super.possible(effector, target) && !target.isOpen
  }

  override def apply (effector : Effector, target : Target) 
  : (Outcome, Double) = {
    val success = target.open
    val message = if (success) "Vous ouvrez la porte. "
    else "La porte est déjà ouverte."
    (Outcome(success, message), 0.5)
  }
}

class CloseChange extends Change {
  val name = "close"

  type Effector = Entity with Humanoid
  type Target = Entity with Openable

  override def possible (effector : Effector, target : Target) = {
    super.possible(effector, target) && target.isOpen
  }

  override def apply (effector : Effector, target : Target)
  : (Outcome, Double) = {
    val success = target.close
    val message = if (success) "Vous fermez la porte. "
    else "La porte est déjà fermée."
    (Outcome(success, message), 0.5)
  }
}

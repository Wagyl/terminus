package changes

import objects._

class SkipChange extends Change {

  val name = "Skip"

  type Effector = Entity with Able
  type Target = Entity

  override def apply (effector : Effector, target : Target)
  : (Outcome, Double) = (Outcome(true, ""), 1.0)
}

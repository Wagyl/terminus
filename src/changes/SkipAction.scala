package changes

import objects._

class SkipAction extends Action {

  val name = "Skip"
  val description = "Passer son tour."
  val identifier = 0

  var actor : Option[Entity with Able] = None


  addEffect ( new Effect {
    val change = new SkipChange
    def effector = actor.get
    val target = Entity.blank
  })

  override def assign (a : Entity) : (Boolean, String) =
    a match {
      case x : Able => 
      actor = Some(x)
      return (true, "Vous passez votre tour.")
      case _ => return (false, "Vous ne pouvez pas passer votre tour.")
    }

  override def abort () : Unit = {
    actor = None
  }

  override def time (a : Entity) : Double = 1.0

}

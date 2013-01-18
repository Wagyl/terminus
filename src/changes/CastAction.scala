package changes

import objects._
import combat._
import representation._
import data._

/* TO DO */
class CastAction(trgt: Victimizable) extends Action {
  val name = ""
  val description = ""
  val identifier = Identifier.attack
  val that = this
  var actor: Entity with Creature = null



  addEffect ( new Effect {
    val change = new CastChange
    def effector = actor
    def target = trgt
  })


  def assign(a: Entity): (Boolean, String) =
    a match {
      case x: Entity with Creature =>
        actor = x
	val relation = x.relation(trgt)
	if (relation == Ally ())
	  (false, "Vous etes allié avec cette créature.")
	else {
	  val reach = Action.reach(actor, trgt)
	  if (!reach._1)
	    reach
	  else
	    (true, "Sort lancé !")
	}
      case _ => (false, "Vous ne pouvez pas lancer de sort.")
    }

  def abort () : Unit = { actor = null; }

  override def time (a: Entity) =
    a match {
      case x : Creature => x.defaultSpell.castTime
      case _ => 0
    }
}

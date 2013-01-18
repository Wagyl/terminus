package changes

import objects._

/*
 * Composition logique de plusieurs Effects.
 */


/*
 * Séquencement.
 * Le deuxième Effect n'est accomplit que si le premier a réussi.
 */
final case class Sequence (val first : Effect, val second : Effect)
extends Effect {

  override val change = new Change {
    val name = ""
    type Effector = Blank
    type Target = Blank
    def apply (e : Effector, t : Target) = null
  }
  override def effector = Entity.blank
  override def target = Entity.blank
  

  override def exec () : Outcome = {
    val o1 = first.exec
    if (o1.success) {
      val o2 = second.exec
      Outcome(o2.success, o1.message + " " + o2.message)
    }
    else
      o1
  }

  override def modify () : Effect = { this }
}


/*
 * Branchement conditionnel.
 */
abstract case class ConditionalBranch 
(val thenEffect : Effect, val elseEffect : Effect) extends Effect {

  override val change = new Change {
    val name = ""
    type Effector = Blank
    type Target = Blank
    def apply (e : Effector, t : Target) = null
  }
  override def effector = Entity.blank
  override def target = Entity.blank

  /*
   * La condition.
   * Doit être redéfinie.
   */

  def condition : Boolean


  override def exec () : Outcome = {
    if (condition)
      thenEffect.exec
    else
      elseEffect.exec
  }


  override def modify () : Effect = { this }

}

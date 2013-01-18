package changes

import objects._

abstract class Effect {

  val change : Change
  def effector : change.Effector
  def target : change.Target


    /*
     * Mécanismes internes à l'Effect :
     * ne devraient pas nécessiter de redéfinition.
     * */

  type Amendment = Effect => Effect

  /* Détermine si la notification doit être effectuée. */
  private var notice : Boolean = true
  /* Détermine si le changement doit notifier son effector. */
  var notifyEffector : Boolean = true
  /* Détermine si le changement doit notifier son target. */
  var notifyTarget : Boolean = true

 /*
   * Applique le changement. Ne doit pas être redéfinie.
   * La valeur de retour est le temps du changement pour l'effector.
   * None signifie que le changement n'a pu être opéré.
   */
  def exec () : Outcome = {
    if (!change.possible(effector, target)) {
      println("CHANGE : IMPOSSIBLE TO APPLY")
      Outcome(false, "")
    }
    else {
      
      if (notice) {
	notice = false;
	val updatedChange = modify()
	if (updatedChange != this)
	  return updatedChange.exec
      }

      val (outcome, diffTime) = change (effector, target)

      notice = true;
      
      if (outcome.success)
	effector match {
	  case e : Able => e.spendTime(diffTime)
	  case _ => ;
	}
      
      outcome
    }
  }


  /*
   * Notification des objets concernés et récupération des modifications
   * éventuelles de l'Effect.
   */
  def modify () : Effect = {
    var f : Amendment = Filter.identity
    if (notifyEffector)
      f = f compose effector.modify(this)
    if (notifyTarget)
      f = f compose target.modify(this)
    
    f(this)
  }
}



object Effect {

  /*
   * Constructeur de Effect.
   */

  def get [E <: Entity, T <: Entity] 
  (c : Change {type Effector >: E <: Entity; type Target >: T <: Entity},
    e : () => E, t : () => T) : Effect = {

    new Effect {
      val change = c
      def effector = e ()
      def target = t ()
    }
  }

}

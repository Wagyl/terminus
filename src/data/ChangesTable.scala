package data

import changes.Effect

/*
 * Table des changements en cours.
 */


abstract class EffectsTable {

  /*
   * Changements immédiats (doivent être opérés dès que possible)
   */
  def addInstantEffect (e : Effect) : Unit

  /*
   * Récupération des changements en attente.
   * ATTENTION, cet appel vide la liste : il faut donc obligatoirement
   * effectuer les changements récupérés.
   */
  def getInstantEffects () : List[Effect]

}


object EffectsTable {

  def create () = { new RealEffectsTable() }

}



class RealEffectsTable extends EffectsTable {

  var immediateEffects : List[Effect] = List[Effect]()


  override def addInstantEffect (e : Effect) : Unit = {
    immediateEffects ::= e
  }

  override def getInstantEffects () : List[Effect] = {
    val rv = immediateEffects.reverse
    immediateEffects = List[Effect]()
    rv
  } 
}

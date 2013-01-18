package changes

import objects._
import log._

/*
 * Toute modification de l'état du jeu est représentée par une sous-classe
 * de Change.
 * Un Change est une modification de l'état de deux objets uniquement :
 * effector et target.
 */

abstract class Change extends Logable {
  type Amendment = Change => Change

  /*
   * Les deux objets concernés par le Change.
   */
  type Effector <: Entity
  type Target <: Entity

  /*
   * À redéfinir pour tout changement concret.
   * La valeur de retour est la sortie du changement et le temps dépensé
   * par le changement par l'effector, s'il est Able.
   */
  def apply (effector : Effector, target : Target) : (Outcome, Double)


  /*
   * Détermine si l'action est possible. Éventuellement redéfinie.
   */
  def possible (effector : Effector, target : Target) : Boolean = {
    (effector != null && target != null)
  }

  /* Nom du changement */
  val name : String
  
  /* LOG */
  def log = "CHANGE"

}


case class Outcome (
  success : Boolean,
  message : String
) { }

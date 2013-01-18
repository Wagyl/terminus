package changes

import objects._
import log._
import representation._
import data._

abstract class Action extends Logable {
  val name : String
  def description : String
  val identifier : Int

  def log = "ACTION"

  /* Système des Changes de l'action. */

  /* Liste des conséquences de l'action. */
  private var _effects : List[Effect] = List()

  /* Renvoie la liste des Changes */
  def effects : List[Effect] = _effects

  /* Ajouter dynamiquement une conséquence. */
  def addEffect (e : Effect) : Unit = {
    _effects = e :: _effects
  }

  /* Méthodes à redéfinir... */

  /*
   * Cette méthode doit effectuer tout le travail nécessaire préalable à la
   * bonne exécution de l'action.
   * Si elle renvoie (true, _), alors l'action doit pouvoir être lancée sans
   * erreur.
   */
  def assign (a : Entity) : (Boolean, String)

  /*
   * Annule toute opération effectuée par assign.
   */
  def abort () : Unit

  /*
   * Retourne une estimation du temps que prendra l'action, à destination
   * du joueur.
   */
  def time (a: Entity) : Double


  /* Mécanismes de l'action. */

  /*
   * Vérifier si l'action est possible.
   * Peut éventuellement être redéfinie.
   */
  def possible (a : Entity) : Boolean = {
    val rv = assign(a)._1
    abort ()
    rv
  }

  /*
   * Exécution d'un change.
   */
  private final def execChange (output : Outcome, e : Effect) : Outcome = {
    var outcome = e.exec
    Outcome(output.success || outcome.success,
      output.message + outcome.message)
  }

  /* Exécuter les conséquences. */
  protected def exec (effects : List[Effect]) : Outcome = {
    effects.foldLeft(Outcome(true, ""))(execChange)
  }

  /* Accomplir l'action. Renvoie false si l'action n'a pu être accomplie. */
  final def perform (a : Entity) : Result = {
    val prelude = assign(a)
    var result : Result= 
      if (prelude._1)  {
	val output : Outcome = exec (effects)
	if (output.success)
	  Success(prelude._2 + output.message)
	else
	  Fail(output.message)
      }
      else
	Fail(prelude._2)
    

    val instantEffects : List[Effect] = a.game.effectsTable.getInstantEffects ()
    if (instantEffects.nonEmpty) {
      val bonus : Outcome = exec(instantEffects)
      result.description += ("\n" + bonus.message)
    }

    abort ()
    result
  }


  /*
   * Retourne la représentation de l'action à destination de
   * l'interface utilisateur.
   */
  def representation(observer: Entity): Option[ActionInterface] = {
    val that = this
    val contains = observer match {
      case a: Able => a.contains(this)
      case _ => false
    }
    if (possible(observer) || contains) {
      val rep = new ActionInterface(name, identifier) {
        var description = that.description
        var time = Representation.round(that.time(observer), 2)
      }
      Some(rep)
    } else
      None
  }
}



object Action {
  

  /*
   * Pour les Actins s'effectuant sur une cible sur la carte, cette fonction
   * vérifie que la cible est à portée (et formate le résultat comme le retour
   * de la méthode assign).
   * d est la portée de l'action (en nombre de case).
   * Le rayon d'action est ici un carré autour de l'actor.
   */
  def reach (actor : Entity, target : Entity, d : Int) : (Boolean, String) =
    (actor.position, target.position) match {
      case (Coordinates(x1, y1, z1), Coordinates(x2, y2, z2)) =>
      if (z1 != z2)
	return (false, "Vous n'êtes pas sur la même carte que la cible.")
      else if (x1 - x2 > d || x1 - x2 < -d || y1 - y2 > d || y1 - y2 < -d)
	return (false, "Vous n'êtes pas suffisamment près de la cible.")
      else if (x1 == x2 && y1 == y2)
	return (false, "Vous êtes sur la cible.")
      else
	return (true, "")

      case (_, Coordinates(_,_,_)) => 
      return (false, "Vous n'êtes pas sur une carte.")
      case _ =>
      return (false, "La cible n'est pas sur la carte.")
    }

  /*
   * reach pour une distance de 1 case.
   */
  def reach (actor : Entity, target : Entity) : (Boolean, String) = {
    reach (actor, target, 1)
  } 
}

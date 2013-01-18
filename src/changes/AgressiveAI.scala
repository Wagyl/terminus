package changes

import objects._
import data._
import representation._


case class EndOfTime () extends Exception

class AgressiveAI (val automaton : Creature) extends AI {


  var goal : Option[Coordinates] = None


  /* **************************************************************************
   *                          Exécution d'actions.
   * ************************************************************************ */

  /*
   * Exécute l'action donnée, si possible. Renvoie true si l'action
   * a bien été tentée.
   * Lance l'exception EndOfTime si la créature n'a pas assez de temps
   * pour exécuter l'action.
   */
  def perform (action : Option[Action]) : Boolean =
    action match {
      case Some(action) =>
	
      if (action.time(automaton) > 2 * automaton.diffTime)
	throw EndOfTime ()
    
      else {
	if ( action.possible(automaton) ) {
	  action.perform(automaton)
	  true
	}
	else
	  false
      }
      
      case None => false
    }

  /*
   * Exécute la première action possible d'une liste d'actions.
   * Renvoie true si une action de la liste a bien été tentée.
   */
  def perform (actions : List[Action]) : Boolean = {
    val first = actions.find(_.possible(automaton))
    perform(first)
  }


  /* **************************************************************************
   *                           Fonctions animate.
   * ************************************************************************ */


  def animate (a : Creature) : Unit = {
    if (this.automaton == a)
      animate ()
    else
      println("ERREUR : AgressiveAI : le demandeur n'est pas le propriétaire.")
  }

  def animate () : Unit = {
    try {
      animate_aux
    } catch {
      case EndOfTime () => return
    }
    
  }

  def animate_aux () : Unit = {
    var rv : Boolean = false

    val character = automaton.game.character
    if (automaton.distance(character) < 2) {
      val attack = character.useByIdent(Identifier.attack)
      rv = perform(attack)
    }

    if (rv)
      return animate ()

    goal match {
      case Some(c) => if (automaton.distance(c) < 2) goal = None
      case _ =>
    }

    selectGoal

    if (goal != None)
      rv = moveToGoal

    if (rv) 
      return animate ()

    else {
      if (scala.util.Random.nextInt(2) > 0)
	rv = manipulate

      if (rv) 
	return animate ()
      
      else
	circling ()
    }

    return animate ()
  }

  /* **************************************************************************
   *                           Objectif.
   * ************************************************************************ */

  /*
   * Sélection d'un objectif.
   */
  def selectGoal () : Unit = {
    val character = automaton.game.character

    // si l'automate voit un personnage, ce dernier est son objectif
    if (automaton.see(character) && automaton.relation(character) == Enemy ())
      character.position match {
	case c : Coordinates => goal = Some(c) ;
	case _ => ;
      }

    // sinon, objectif aléatoire
    if (goal == None)
      automaton.position match {
	case Coordinates(x, y, z) =>
	val dx = scala.util.Random.nextInt(11) + 2
	val dy = scala.util.Random.nextInt(11) + 2
	goal = Some(Coordinates(x+dx, y+dy, z))
	
	case _ => goal = None
      }
  }
  
  def moveToGoal () : Boolean = {
    (automaton.position, goal) match {
      case (c : Coordinates, Some(g)) =>
      val idActions = moveto(c, g)
      val actions = idActions.map(x => automaton.ability(x)).flatMap(x => x)
      return perform(actions)

      case (_, _) => false
    }
  }



  /*
   * Renvoie une liste des identifiants des actions de déplacements à tenter
   * pour se rentre au point dest depuis le point position.
   */
  def moveto (position : Coordinates, dest : Coordinates) : List[Int] = {
    
    val dx = dest.x - position.x
    val dy = dest.y - position.y

    if (dx > 0 && dy >= 0)
      List(Identifier.moveTopRight, Identifier.moveRight, Identifier.moveTop, 
	Identifier.moveBottomRight, Identifier.moveTopLeft)
    else if (dx >= 0 && dy < 0)
      List(Identifier.moveBottomRight, Identifier.moveBottom, 
	Identifier.moveRight, Identifier.moveBottomLeft, Identifier.moveTopRight)
    else if (dx < 0 && dy <= 0)
      List(Identifier.moveBottomLeft, Identifier.moveLeft, Identifier.moveBottom,
	Identifier.moveBottomRight, Identifier.moveTopLeft)
    else if (dx <= 0 && dy > 0)
      List(Identifier.moveTopLeft, Identifier.moveLeft, Identifier.moveTop,
	Identifier.moveBottomLeft, Identifier.moveTopRight)
    else
      List()
  }



  /* **************************************************************************
   *                           Actions aléatoires.
   * ************************************************************************ */


  /*
   * Accomplit une action proposée par un Usable à portée, au hasard.
   */
  def manipulate () : Boolean = {
    automaton.position match {
      case c : Coordinates =>
      val entities = automaton.game.data.getAround(c, 1)
      val entitiesUsables = entities.filter(_.isInstanceOf[Usable])
      val usables = entitiesUsables.map(_.asInstanceOf[Usable])
      val uses = usables.flatMap(_.getUses).filter(_.possible(automaton))

      if (uses.length > 0) {
	val r = scala.util.Random.nextInt(uses.length)
	return perform(Some(uses(r)))
      }
      else
	false

      case _ => false
    }
  }

  /*
   * Accomplit une action au hasard.
   */
  def circling () : Boolean = {
    val actions = automaton.getAbilities
    if (actions.length > 0) {
      val r = scala.util.Random.nextInt(actions.length)
      return perform(Some(actions(r)))
    }
    else
      false
  }

}

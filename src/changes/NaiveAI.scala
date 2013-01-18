package changes

import objects._
import data._
import representation._

/*
 * Une implémentation minimale.
 */

class NaiveAI extends AI {

  def extractUses (pair : (Int, EntityInterface)) 
  : List[(Int, (Int, ActionInterface))] =
    pair._2 match {
      case o : ObjectInterface =>
      val uses = o.uses
      uses.map(x => (pair._1, x))
      case _ => List()
  }

  
  def animate (automaton : Creature) : Unit = {
    automaton.position match {
      case c : Coordinates =>

      val character = automaton.game.character
      if (automaton.see(character))
	()

      val actions = automaton.getAbilities

      val entities = automaton.game.data.getAround(c , 1)
      val rep = Representation.get(automaton, entities)
      val uses = rep.flatMap(x => extractUses(x))

     perform(automaton, actions, uses, entities)
      
      case _ => ;
    }
  }

  def perform (automaton : Creature, a : List[Action], 
    u : List[(Int, (Int, ActionInterface))], entities : List[Entity]) : Unit = {

    var actions = a
    var uses = u

    if (actions.size + uses.size == 0)
      return

    val max = actions.size + uses.size
    var r = scala.util.Random.nextInt(max)

    val action = 
      if (r < actions.size) {
	actions(r)
      }
      else {
	val u = uses(r - actions.size)
	val obj = entities(u._1)
	obj match {
	  case usable : Usable => 
	  usable.getUse(u._2._1)
	  case _ => println("ERREUR NAiveAi 1"); null
	}
	}

      if (actions == null)
	return
	
      if (action.time(automaton) > 2 * automaton.diffTime)
	return
      
      if ( action.possible(automaton) ) {
	action.perform(automaton)
      } else {
	actions = actions.diff(List(action))
	uses = uses.diff(List(action))
      }

      perform(automaton, actions, uses, entities)
  }
  
}


object NaiveAI {
  val aI : NaiveAI = new NaiveAI

  def get : NaiveAI = aI
  
}

package changes

import objects._

/*
 * Classe abstraite des intelligences artificielles.
 */

abstract class AI {

  def animate (character : Creature) : Unit

}


object AI {

  def get (automaton : Creature, aim : Aim) : AI = aim match {
    case Naive () => NaiveAI.get
    case Agressive () => new AgressiveAI(automaton)
  }

}


sealed abstract class Aim {}
case class Naive () extends Aim { }
case class Agressive () extends Aim { }

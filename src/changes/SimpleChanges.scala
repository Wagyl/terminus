package changes

import objects._
import combat._
import representation.Identifier

/*
 * Mocking class.
 * Ne devrait plus être utilisée.
 */
class Eat extends Change {
  val name = "Eat"

  type Effector = Entity with Creature
  type Target = Entity with Eatable

  override def apply  (effector : Effector, target : Target) 
  : (Outcome, Double) = {
    effector.addHealth(target.diffHp)
    (Outcome(true, ""), 1)
  }
}


/*
 * Changement "blanc" :
 * permet de passer un message dans le result d'une Action
 * et de diminuer le temps de l'effector.
 */
class BlankChange (val time : Double, val message : String) 
extends Change  {
  val name = "BlankChange"
  
  type Effector = Entity
  type Target = Entity

  override def apply (effector : Effector, target : Target) 
  : (Outcome, Double) = {
    (Outcome(true, message), time)
  }
}


/*
 * Effets statiques sur des caractéristiques de Creature.
 */

class StaticLifeChange (val diffHealth : Int) extends Change {
  val name = "LifeChange"
  
  type Effector = Entity with Creature
  type Target = Entity

  override def apply (effector : Effector, target : Target) 
  : (Outcome, Double) = {
    if (diffHealth > 0) {
      val res = effector.addHealth(diffHealth)
      (Outcome(true, "Vous avez regagné " + res + " points de vie"), 0)
    }
    else if (diffHealth < 0) {
      val res = effector.subHealth(- diffHealth)
      (Outcome(true, "Vous avez perdu " + res + " points de vie"), 0)
    }
    else
      (Outcome(false, ""), 0)
  }
}


class DrinkChange extends Change {

  val name = "DrinkChange"
  
  type Effector = Creature
  type Target = Waterproof

  override def apply (effector : Effector, target : Target) 
  : (Outcome, Double) = {
    val success = target.drink
    (Outcome(success, ""), 1.0)
  }
}


class TransferLiquor extends Change {
  val name = "Transfer Liquor"
  
  type Effector = Potable
  type Target = Waterproof

  override def apply (effector : Effector, target : Target)
  : (Outcome, Double) = {
    val liquor = effector.liquor
    val success = target.fill(liquor, effector.drinkMessage)
    (Outcome(success, ""), 0)
  }
  
}


/*
 * CastChange.
 */
class CastChange extends Change {
  val name = "cast"

  type Effector = Entity with Creature
  type Target = Entity with Life with StatHandler

  override def apply (effector : Effector, target : Target) :
  (Outcome, Double) = {
    val fightRes = Fight.handleFight(effector, target, effector.defaultSpell)
    val diff = target.subHealth(fightRes.res)
    target match {
      case x : Memory => x.addNotice(Identifier.healthChange, Identifier.hit, "La créature " + effector.name + " vous a frappé. Vous avez perdu " + -diff + " points de vie.")
      case _ => ;
    }
    (Outcome(true, fightRes.toString), fightRes.time)
  }
}

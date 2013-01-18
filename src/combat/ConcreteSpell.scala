package combat

import objects._

/* Des Spell concrêts */
object MagicSpell extends Spell {
  val name = "magical spell"
  val value = 10
  val castTime = 1.5
  val damageType = MagicDamage()

  override def defaultRatio: Int = 1

  addSpecialRatio({ case x: MagicPower => 2 })
}

object PhysicalSpell extends Spell {
  val name = "physical spell"
  val value = 15
  val castTime = 1.5
  val damageType = PhysicalDamage()

  override def defaultRatio: Int = 1
}

object CompSpell extends CompositeSpell(MagicSpell, PhysicalSpell)
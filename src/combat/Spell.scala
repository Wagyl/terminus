package combat

import objects._
import log._

/* Trait représentant "l'interface" des spell */
trait SpellDef extends Logable {
  val name: String
  val value: Int
  val castTime: Double

  type T = Entity with StatHandler

  def cast(effector: T, target: T, sheet: CombatSheet): CombatResult
}

/* Classe abstraite des spell "leaf" */
abstract class Spell extends SpellDef {
  val damageType: DamageType

  var lastDamageDone: CombatResult = null
  var statRatio: P = { case _ => defaultRatio }

  type P = PartialFunction[Stat, Int]

  def defaultRatio: Int

  def addSpecialRatio(toAdd: P): Unit = statRatio =
    toAdd orElse statRatio

  def log = value + " modified to " + lastDamageDone + " - " + damageType.name

  def cast(effector: T, target: T, sheet: CombatSheet): CombatResult = {
    //val sheet = new BasicCombatSheet
    sheet.baseValue = value
    sheet.castTime = castTime
    /* Prise en compte des stat de l'effector */
    effector.statSet.handler.foreach {
      stat =>
        stat match {
          case x: Stat with OffensiveStat =>
            x.offensiveHandle(effector, target, damageType, sheet, statRatio(x))
          case _ =>
        }
    }
    /* Prise en compte des stat de la target */
    target.statSet.handler.foreach {
      stat =>
        stat match {
          case x: Stat with DefensiveStat =>
            x.defensiveHandle(effector, target, damageType, sheet, statRatio(x))
          case _ =>
        }
    }
    lastDamageDone = sheet.compute
    lastDamageDone
  }
}

/* Classe abstraite des spell "composite" */
abstract case class CompositeSpell(children: Spell*) extends SpellDef {
  var tmpName: String = ""
  var tmpValue: Int = 0
  var tmpTime: Double = 0
  for (leaf <- children) {
    tmpName += leaf.name + " ";
    tmpValue += leaf.value;
    tmpTime += leaf.castTime
  }
  val name = "CompositeSpell(" + tmpName + ")"
  val value = tmpValue
  val castTime = tmpTime

  def log = ""

  def cast(effector: T, target: T, sheet: CombatSheet): CombatResult = {
    val res = for (a <- children) yield a.cast(effector, target, sheet)
    CompositeCombatResult(res.toList)
  }
}
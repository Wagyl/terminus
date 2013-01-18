package combat

import objects._

/* Trait ajoutant le support des stat */
trait StatHandler extends Entity {
  var statSet: StatSet = new StatSet()
}

/* Classe de gestion des stat */
class StatSet {
  var handler: List[Stat] = List()

  def addStat(s: Stat): Unit = handler = s :: handler
}

/* Toutes les stats doivent étendre cette classe et définir le trait associé */
abstract class Stat(v: Int) {
  val name: String

  var value: Int = v

  def modifyValue(newValue: Int) = value = newValue

  type T = Entity with StatHandler
  type P = PartialFunction[(DamageType, CombatSheet, T, T, Int), Unit]
}

/* Stat offensive : à prendre en compte pour l'effector lors d'un cast */
trait OffensiveStat extends Stat {
  var offensiveParts: P = { case _ => }

  def addOffensivePart(toAdd: P): Unit =
    offensiveParts = toAdd orElse offensiveParts

  def offensiveHandle(effector: T, target: T,
                      dmgType: DamageType, sheet: CombatSheet,
                      ratio: Int): Unit =
    offensiveParts(dmgType, sheet, effector, target, ratio)
}

/* Stat défensive : à prendre en compte pour la target lors d'un cast */
trait DefensiveStat extends Stat {
  var defensiveParts: P = { case _ => }

  def addDefensivePart(toAdd: P): Unit =
    defensiveParts = toAdd orElse defensiveParts

  def defensiveHandle(effector: T, target: T,
                      dmgType: DamageType, sheet: CombatSheet,
                      ratio: Int): Unit =
    defensiveParts(dmgType, sheet, effector, target, ratio)
}

package combat

import objects._

/* Quelques traits concrêts des feuilles de combat */
trait FlatScaling {
  var flatScaling: Int
}

trait PrcScaling {
  var prcScaling: Int
}

trait FlatMitigation {
  var flatMitigation: Int
}

trait PrcMitigation {
  var prcMitigation: Int
}

trait HitHandling {
  var hitHandling: Int
}

/* Une feuille de combat concrête */
class BasicCombatSheet extends CombatSheet with FlatScaling with PrcScaling
  with FlatMitigation with PrcMitigation with HitHandling {
  var baseValue: Int = 0
  var castTime: Double = 0
  var flatScaling: Int = 0 // Augmentation linéaire des dégats
  var prcScaling: Int = 0 // Augmentation par pourcentage (s'applique après linéaire)
  var flatMitigation: Int = 0 // Réduction linéaire des dégats
  var prcMitigation: Int = 0 // Réduction par pourcentage des dégats (en dernier)
  var hitHandling: Int = 0

  def compute = {
    var res = 0
    if (hasHit) {
      res = ((baseValue + flatScaling) * (1 + prcScaling) - flatMitigation) *
        (1 - prcMitigation)
      if (res > 0)
        Hit(res, castTime)
      else
        Hit(0, castTime)
    } else {
      Miss(res, castTime)
    }
  }

  def hasHit = scala.util.Random.nextInt(100) < hitHandling
}

/* Quelques résultats concrêts d'un combat */
case class Hit(value: Int, timeCost: Double)
  extends CombatResult(value, timeCost) {
  override def toString: String = "Hit : " + super.toString
}
case class Miss(value: Int, timeCost: Double)
  extends CombatResult(value, timeCost) {
  override def toString: String = "Miss : " + super.toString
}
package combat

/* Tous les types de dégat doivent étendre cette classe */
abstract class DamageType(n: String) {
  val name: String = n
}

/* Des types de dégats concrêts */
case class MagicDamage() extends DamageType("magic damage")

case class PhysicalDamage() extends DamageType("physical damage")

package objects
import combat.StatHandler
import combat.MagicPower
import combat.HitStat
import representation.Identifier

/* Pièce de tête */
class HeadItem extends Entity with Edible with Stuff {
  val name = "SimpleHead"
  val description = "Simple Head Item."
  val identifier = Identifier.fountain
  val pieces = List(new Head)
  val magicPower: Int = 2
  val hit: Int = 2

  override def canEquip(e: Equipment): Boolean = true

  override def equip(e: Equipment): Boolean =
    e match {
      case x: StatHandler =>
        for (st <- x.statSet.handler)
          st match {
            case s: MagicPower =>
              s.modifyValue(magicPower)
            case s: HitStat =>
              s.modifyValue(hit)
              true
            case _ => ()
          }
        true
      case _ => false
    }

  override def unequip(e: Equipment): Boolean =
    e match {
      case x: StatHandler =>
        for (st <- x.statSet.handler)
          st match {
            case s: MagicPower =>
              s.modifyValue(-magicPower)
            case s: HitStat =>
              s.modifyValue(-hit)
              true
            case _ => ()
          }
        true
      case _ => false
    }
}
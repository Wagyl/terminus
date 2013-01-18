package combat

import objects._

/* Magic Resist */
class MagicResist(v: Int) extends Stat(v) with DefensiveStat {
  val name = "magic resist"
  value = v

  addDefensivePart({
    case (dT: MagicDamage, cS: CombatSheet with FlatMitigation,
      _, _, ratio) => cS.flatMitigation += ratio * value
  })
}

trait MagicResist_ extends StatSet {
  def defaultMagicResist: Int
  addStat(new MagicResist(defaultMagicResist))
}

/* Magic Power */
class MagicPower(v: Int) extends Stat(v) with OffensiveStat {
  val name = "magic power"
  value = v

  addOffensivePart({
    case (dT: MagicDamage, cS: CombatSheet with FlatScaling,
      _, _, ratio) => cS.flatScaling += ratio * value
  })
}

trait MagicPower_ extends StatSet {
  def defaultMagicPower: Int
  addStat(new MagicPower(defaultMagicPower))
}

/* Armor */
class Armor(v: Int) extends Stat(v) with DefensiveStat {
  val name = "armor"
  value = v

  addDefensivePart({
    case (dT: PhysicalDamage, cS: CombatSheet with FlatMitigation,
      _, _, ratio) => cS.flatMitigation += ratio * value
  })
}

trait Armor_ extends StatSet {
  def defaultArmor: Int
  addStat(new Armor(defaultArmor))
}

/* Hit */
class HitStat(v: Int) extends Stat(v) with OffensiveStat {
  val name = "hit"
  value = v

  addOffensivePart({
    case (_, cS: CombatSheet with HitHandling,
      _, _, ratio) => cS.hitHandling += ratio * value
  })
}

trait HitStat_ extends StatSet {
  def defaultHitStat: Int
  addStat(new HitStat(defaultHitStat))
}
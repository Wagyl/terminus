package objects

import representation._
import combat._

class ConcretePlayer(
  name: String,
  description: String) extends Player(name, description) {}

class Warrior(name: String, descr: String) extends Character(name, descr, 100) {
  override val classIdentifier = Identifier.warrior

  statSet =
    new StatSet with MagicResist_ with MagicPower_ with Armor_ with HitStat_ {
      def defaultMagicResist = 5
      def defaultMagicPower = 2
      def defaultArmor = 3
      def defaultHitStat = 90
    }
}

class Sorcerer(name: String, descr: String) extends Character(name, descr, 70) {
  override val classIdentifier = Identifier.sorcerer

  statSet =
    new StatSet with MagicResist_ with MagicPower_ with Armor_ with HitStat_ {
      def defaultMagicResist = 7
      def defaultMagicPower = 5
      def defaultArmor = 1
      def defaultHitStat = 75
    }
}

class Rogue(name: String, descr: String) extends Character(name, descr, 85) {
  override val classIdentifier = Identifier.rogue

  statSet =
    new StatSet with MagicResist_ with MagicPower_ with Armor_ with HitStat_ {
      def defaultMagicResist = 6
      def defaultMagicPower = 4
      def defaultArmor = 5
      def defaultHitStat = 80
    }
}
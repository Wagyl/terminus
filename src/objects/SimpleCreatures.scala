/*
 * Quelques classes de créatures.
 */

package objects

import changes._
import combat._
import representation._

/*
 * Goblin...
 */
class Goblin(val baseHealth: Float) extends Creature with Humanoid with Controlable {

  addCoteries(Monster ())
  addRelation(Animals (), Enemy ())

  lazy val name: String = "goblin"
  val description: String = "Un gobelin"
  val identifier = Identifier.goblin
  val defaultSpell = MagicSpell
  val eyesight = 4

  override val aI: AI = AI.get(this, Agressive())

  statSet =
    new StatSet with MagicResist_ with MagicPower_ with Armor_ with HitStat_ {
      def defaultMagicResist = 5
      def defaultMagicPower = 2
      def defaultArmor = 3
      def defaultHitStat = 50
    }
}

object Goblin {
  def get(): Goblin = {
    val random: Float = scala.util.Random.nextFloat()
    val health: Float = 40 + 20 * random
    new Goblin(health)
  }
}

/*
 * Snake...
 */
class Snake(val baseHealth: Float) extends Creature with Controlable with Animal {

  addCoteries(Monster (), Animals ())

  lazy val name = "snake"
  val description = "Un serpent"
  val identifier = Identifier.snake
  val defaultSpell = PhysicalSpell
  val eyesight = 3

  statSet = new StatSet with Armor_ {
    def defaultArmor = 1
  }
}

object Snake {
  def get(): Snake = {
    val random: Float = scala.util.Random.nextFloat()
    val health: Float = 20 + 10 * random
    new Snake(health)
  }
}

package objects

import changes._

/*
 * Trait des Entity vivantes.
 * (Bénéficiant de points de vie.)
 */

trait Life extends Entity {
  /*
   * La valeur baseHealth est un critère de détermination des valeurs
   * initiales de health (health sera proportionnel à baseHealth).
   */
  val baseHealth: Float

  private var _health: Int = baseHealth.toInt
  private var _maxHealth: Int = baseHealth.toInt

  def health: Int = _health

  def addHealth (diff : Int) : Int = {
    val before = health
    if (diff > 0)
      _health = scala.math.min(_maxHealth, health + diff)
    health - before
  }

  def subHealth (diff : Int) : Int = {
    val before = health
    if (diff > 0) {
      _health = scala.math.max(0, health - diff)
      if (_health <= 0) {
	val that = this
        this.game.effectsTable.addInstantEffect(new Effect 
	  { type Effector = Entity
	    type Target = Life
	    val change = new DeathChange
	    val effector = Entity.god
	    val target = that
	  })
      }
    }
    health - before
  }
}

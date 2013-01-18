package data

import objects.Creature

/*
 * Classe concrète de Game.
 */

class RealGame extends Game {

  var player : Creature = null

  override val world = new RealWorld (this)
  
  override val data = Data.create (this)

  override val effectsTable = EffectsTable.create ()
  
  override def character = player

  override def setCharacterAt(pl : Creature, position : Coordinates) {
    pl.game = this
    player = pl
    data.addEntityAt(pl, getClosetStackableCoordinate(position))
  }

  override def setCharacter(pl: Creature) {
    player = pl
  } 
}

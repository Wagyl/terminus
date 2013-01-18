package data

import objects.Creature
import objects.Goblin

trait HotSeatGame extends RealGame {
  /**
   * Liste des joueurs dans la partie
   */
  protected var players: Array[Creature] = Array()

  /**
   * Joueur courant
   */
  protected var currentPlayerIdx: Int = 0
  
  /* Ajouter un nouveau joueur */
  val newPlayer: Creature = Goblin.get()
  newPlayer.active = false
  this.players :+= newPlayer
  
  def nextPlayer(): Creature = {
    if (this.players != null) {
      this.currentPlayerIdx = (this.currentPlayerIdx + 1) % (this.players.length)
      super.setCharacter(this.players(this.currentPlayerIdx))
    }
    this.players(this.currentPlayerIdx)
  }
  
  override def setCharacterAt(pl : Creature, position : Coordinates) {
    super.setCharacterAt(pl, position)
    
    /* Ajouter le premier joueur */
    this.players :+= this.character
    this.currentPlayerIdx = this.players.size - 1
    
    /* Ajouter les autres joueurs sur le terrain */
    this.players foreach (pl => this.data.addEntityAt(pl, getClosetStackableCoordinate(Coordinates(5, 4, Floor(0)))))
  }

  override def setCharacter(pl: Creature) {
    super.setCharacter(pl)
    this.players(this.currentPlayerIdx) = pl
  } 
}
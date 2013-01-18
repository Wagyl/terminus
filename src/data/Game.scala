package data

import objects.Creature
import representation.Identifier
import scala.util.Random

/*
 * Contient toutes les données d'une partie.
 */

abstract class Game {

  /*
   * Les cartes du jeu, constituées d'un Tile par coordonnée.
   */
  def world: World

  /*
   * Les données (Entity non Tile) présent dans world.
   */
  def data: Data

  /*
   * Le joueur.
   */
  def character: Creature

  /*
   * La table des changements en cours.
   */

  def effectsTable: EffectsTable

  /*
   * Placer le joueur.
   */
  def setCharacterAt(character: Creature, position: Coordinates): Unit

  /*
   * Changer le joueur actif. 
   */
  def setCharacter(character: Creature): Unit

  /*
   * Fonctions utilitaires...
   */

  def isTransparent(c: Coordinates): Boolean = {
    val tile = world.getTileAt(c)
    if (tile.isTransparent) {
      val elements = data.getEntitiesAt(c)
      elements.exists(x => !x.isTransparent)
    } else
      false
  }

  def isStackable(c: Coordinates): Boolean = {
    val tile = world.getTileAt(c)
    if (tile.isStackable) {
      val elements = data.getEntitiesAt(c)
      elements.forall(x => x.isStackable)
    } else
      false
  }

  def getClosetStackableCoordinate(c: Coordinates): Coordinates = {
    if (isStackable(c)) {
      c
    } else {
      var newC = c
      var nTry = 0

      val maxHeight = if (this.world.land(newC.z).height < 0) Int.MaxValue else this.world.land(newC.z).height
      val maxWidth = if (this.world.land(newC.z).width < 0) Int.MaxValue else this.world.land(newC.z).width

      while (!isStackable(newC) && nTry < 1000) {
        val x = newC.x + (Random.nextInt(10) - 5)
        val y = newC.y + (Random.nextInt(10) - 5)

        if (x > 0 && x < maxWidth && y > 0 && y < maxHeight) {
          newC = Coordinates(x, y, newC.z)
        }

        nTry += 1
      }
      newC
    }
  }

}

object Game {

  def create(): Game = {
    new RealGame()
  }

  def create(extensionsIds: List[Int]): Game = {
    if (extensionsIds.contains(Identifier.hotSeat)) {
      new RealGame() with HotSeatGame
    } else {
      this.create()
    }
  }
}

package objects

import combat._
import changes._

/*
 * Fichiers dédiés aux classes abtraites des créatures.
 * Les traits ne servent parfois qu'au typage des types de créatures...
 */

/*
 * Trait des créatures.
 */
trait Creature extends MutableEntity with Container with Equipment
  with Life with EyesOnly with Resources with StatHandler 
  with RelationShip with Moving with Victimizable {

    def aI : AI = AI.get(this, Naive())
    var active : Boolean = true
    val defaultSpell: SpellDef

    override def mutation(): Unit = {
      if (active)
	aI.animate(this)
    }
  }

/*
 * Trait des créatures humanoïdes.
 */
trait Humanoid extends Creature {
  override var equipment : List[(Piece, Option[Stuff])] = 
    List((Head (), None), (LeftHand (), None), (RightHand (), None))
}

trait Animal extends Creature {
  override var equipment : List[(Piece, Option[Stuff])] = List()
}

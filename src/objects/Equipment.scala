package objects

import changes.EquipAction
import changes.UnequipAction

/*
 * Entités capable de s'équiper d'objets.
 */

trait Equipment extends Entity {

  var equipment : List[(Piece, Option[Stuff])]

  def pieces () : List[Piece] = equipment.map(_._1)

  /*
   * Attention, fonctionnera mal (ou pas) si plusieurs
   * emplacements identiques...
   */
  private def equip (piece : Piece, item : Stuff) : Boolean = {
    val (e1, e2) = equipment.partition(x => (x._1 == piece))
    e1 match {
      case (p, Some(i)) :: tail if (p == piece) =>
      unequip(i)
      equipment = (piece, Some(item)) :: (tail ::: e2)
      true
      case (p, None) :: tail if (p == piece) => 
      equipment = (piece, Some(item)) :: (tail ::: e2)
      true
      case _ => false
      }
  }

  def equip (item : Stuff) : Boolean = {
    val p = item.pieces
    if (p.forall(x => equipment.contains(x))) {
      p.foreach(x => this.equip(x, item))
      item.equip(this)
      item.equipped = Some(this)
      true
    }
    else
      false
  }

  def unequip (item : Stuff) : Boolean = {
    equipment.map(x => if (x._2 == Some(item)) (x._1, None) else x)
    item.unequip(this)
    item.equipped = None
    true
  }

}


/*
 * Entity pouvant etre équipés.
 */

trait Stuff extends Entity with Edible {

  val pieces : List[Piece]

  addUsage(new EquipAction(this))
  addUsage(new UnequipAction(this))
  
  var equipped : Option[Equipment] = None
  def isEquip () : Boolean = (equipped == None)


  /* A redéfinir dans les classes concrètes */

  def canEquip (e : Equipment) : Boolean
  /*
   * Idéalement, ces deux fonctions devraient etre remplacées par
   * des Changes (mais elles suffiront pour l'instant).
   */
  def equip (e : Equipment) : Boolean
  def unequip (e : Equipment) : Boolean

}



/*
 * Emplacement d'équipement.
 */

sealed abstract class Piece { }
case class Head () extends Piece { }
case class LeftHand () extends Piece { }
case class RightHand () extends Piece { }

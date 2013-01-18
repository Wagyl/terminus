package changes

import objects._
import representation._

/*
 * Action d'équiper un objet.
 */

class EquipAction (e : Stuff) extends Action {

  val item : Stuff = e
  var actor : Option[Equipment] = None

  val name = "Equip"
  val description = "Equip " + item.name
  val identifier = Identifier.equip

  override def time (a : Entity) : Double = 0.1
  
  override def assign (a : Entity) : (Boolean, String) =
    a match {
      case x : Equipment => actor = Some(x)
      if (!Container.contains(x, item))
	(false, "Vous n'avez pas l'objet en votre possession.")
      else {
	val pieces = item.pieces
	if (pieces.forall(z => x.pieces.contains(z))) {
	  if (item.canEquip(x))
	    if (x.equip(item))
	      (true, "Vous avez équipé l'objet " +item.name)
	    else
	      (false, "Vous n'avez pas pu équiper cet objet.")
	  else
	    (false, "Vous ne pouvez pas vous équiper de cet objet.")
	}
	else
	  (false, "Vous ne pouvez pas équiper un tel objet.")
      }

      case _ => return (false, "Vous ne pouvez pas équiper un objet.")
    }
   
  override def abort () = {
    actor = None
  }

}




/*
 * Action de déséquiper un objet.
 */

class UnequipAction (e : Stuff) extends Action {

  val item : Stuff = e
  var actor : Option[Equipment] = None

  val name = "Unequip"
  val description = "Unequip " + item.name
  val identifier = Identifier.unequip

  override def time (a : Entity) : Double = 0.1
  
  override def assign (a : Entity) : (Boolean, String) =
    a match {
      case x : Equipment => actor = Some(x)
      item.equipped match {
	case Some(e) if (e == x) => 
	if (x.unequip(item))
	  (true, "Vous avez retiré l'objet " +item.name)
	else
	  (false, "Vous n'avez pas pu retirer cet objet.")
	case _ => (false, "Vous n'avez pas cet objet d'équipé pour l'instant.")
      }

      case _ => return (false, "Vous ne pouvez pas équiper un objet.")
    }
   
  override def abort () = {
    actor = None
  }

}

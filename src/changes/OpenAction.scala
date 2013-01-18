package changes

import objects._
import representation._
import data._

/*
 * Classe abstraite pour les actions Open et Close.
 */
abstract class DoorAction (d : Entity with Openable) extends Action {

  val door = d
  /* Statut que doit avoir la porte pour que l'action soit possible. */
  val status : Boolean
  var actor : Option[Entity with Humanoid] = None

  /* Message en cas de réussite de l'action. */
  val message : String

  override def time (a : Entity) : Double = 0.5

  def assign (a : Entity) : (Boolean, String) = {
    a match {
      case x : Entity with Humanoid => actor = Some(x)
      case _ => return (false, "Vous ne pouvez ouvrir ou fermer une porte.")
    }
    val reach = Action.reach(a, door)
    if (!reach._1)
      return reach
    if (door.isOpen != status)
      return (false, "La porte est déjà dans l'état désiré.")
    
    if (door.isStackable)
      door.position match {
	case c : Coordinates =>
	if (!door.game.isStackable(c))
	  return (false, "Quelque chose empêche de fermer la porte.")
	// ici, ce cas ne peut plus se produire
	case _ => ;
      }
    
    (true, message)
  }

  def abort () : Unit = {
    actor = None
  }
}

class OpenAction (d : Entity with Openable) extends DoorAction (d) {
  val identifier = Identifier.openAction
  val name = "Open"
  val description = "Open " + door.name

  val status = false
  val message = ""

  addEffect ( new Effect {
    val change = new OpenChange
    def effector = actor.get
    val target = door
  })
}

class CloseAction (d : Entity with Openable) extends DoorAction (d) {
  val identifier = Identifier.closeAction
  val name = "Close"
  val description = "Close " + door.name

  val status = true
  val message = ""

  addEffect ( new Effect {
    val change = new CloseChange
    def effector = actor.get
    val target = door
  })
}

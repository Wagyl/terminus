package changes

import objects._
import representation._


/*
 * Action de boire à un Entity Potable sur la carte.
 */
class DrinkPotableAction (f : Potable) extends Action {

  val potable : Potable = f
  var actor : Option[Creature] = None

  val name = "Drink"
  val description = "Boire à une fontaine."
  val identifier = Identifier.drinkPotable


  override def effects : List[Effect] = {
    val changes = potable.liquor
    val effects = changes.map(x => Effect.get (x, actor.get _, () => potable))
    effects ++ super.effects
  }


  override def time (a : Entity) : Double = 1

  override def assign (a : Entity) : (Boolean, String) = {
    a match {
      case x : Creature => actor = Some(x)
      case _ => return (false, "Vous ne pouvez pas boire.")
    }
    val reach = Action.reach(a, potable)
    if (!reach._1)
      return reach

    (true, potable.drinkMessage)
  }

  override def abort () = {
    actor = None
  }

}


/*
 * action de boire à un contenant présent dans son inventaire.
 */
class DrinkAction (f : Waterproof) extends Action {

  val name = "Drink"
  val description = "Boire à un récipient."
  val identifier = Identifier.drinkAction

  val flask : Waterproof = f
  var actor : Option[Creature] = None

  
  addEffect( Effect.get(new DrinkChange, actor.get _, () => flask) )
  

  override def effects : List[Effect] = {
    val changes = flask.liquor
    val effects = changes.map(x => Effect.get [Creature, Entity] (x, actor.get _, () => flask))
    effects ++ super.effects
  }

  override def time (a : Entity) : Double = 1

  override def assign (a : Entity) : (Boolean, String) = {
    a match {
      case x : Creature => actor = Some(x)
      if (!Container.contains(x, flask))
	(false, "Vous n'avez pas l'objet en votre possession.")
      else
	if (flask.isEmpty)
	  (false, "L'objet est vide.")
	else
	  (true, "")
      case _ => return (false, "Vous ne pouvez pas boire.")
    }
    
  }

  override def abort () : Unit = {
    actor = None
  }
}


class TransferLiquorAction (f : Potable) extends Action {


  val potable : Potable = f
  var actor : Option[Creature] = None
  var container : Option[Waterproof] = None

  val name = "Fill"
  val description = "Remplir un contenant étanche à une fontaine."
  val identifier = 0


  addEffect( Effect.get(new TransferLiquor, potable _, container.get _) )
  

  override def time (a : Entity) : Double = 2

  override def assign (a : Entity) : (Boolean, String) =
    a match {
      case x : Creature => actor = Some(x)

      val reach = Action.reach(x, potable)
      if (!reach._1)
	return reach

      val contents = x.contents.filter(_.isInstanceOf[Waterproof])
      val containers = contents.map(_.asInstanceOf[Waterproof])
      container = containers.find(_.isEmpty)

      container match {
	case Some (_) => (true, "Vous remplissez la bouteille.")
	case None => (false, "Vous n'avez aucun contenant vide en votre possession.")
      }
      case _ => return (false, "Vous ne pouvez pas boire.")
    }

  override def abort () : Unit = {
    actor = None
    container = None
  }


}

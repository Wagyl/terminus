package changes

import objects._
import representation._

class PickUpAction (c : Pickable) extends Action {

  val loot : Pickable = c
  var actor : Option[Entity with Able with Container] = None
  
  val name = "Pick Up"
  def description = "Rammaser un objet."
  val identifier = Identifier.pickUpAction
  
  
  addEffect ( new Effect {
    val change = new PickUpChange
    def effector = actor.get
    val target = loot
  })


  override def time (a : Entity) : Double = 0.5

  override def assign (a : Entity) : (Boolean, String) = {
    a match {
      case x : Entity with Able with Container => actor = Some(x)
      case _ => return (false, "Vous ne pouvez pas ramasser un objet.")
    }
    
    if (a.position != loot.position)
      return (false, "Vous n'êtes pas sur la même case que l'objet.")
    
    (true, "Vous ramassez l'objet.")
  }
  
  override def abort () = {
    actor = None
  }
}


class DropAction (c : Pickable) extends Action {
  
  val loot : Pickable = c
  var actor : Option[Entity with Able with Container] = None
  
  val name = "Drop"
  val description = "Abandonner un objet."
  val identifier = Identifier.dropAction
  

  addEffect ( new Effect {
    val change = new DropChange
    def effector = actor.get
    val target = loot
  })
  
  override def time (a : Entity) : Double = 0.5
  
  override def assign (a : Entity) : (Boolean, String) = {
    a match {
      case x : Entity with Able with Container => actor = Some(x)
      case _ => return (false, "Vous ne pouvez pas lâcher un objet.")
    }
    
    if (!Container.contains(a, loot))
      return (false, "Vous ne possédez pas cet objet.")
    
    (true, "Vous laĉhez l'objet")
  }

  override def abort () = {
    actor = None
  }

}

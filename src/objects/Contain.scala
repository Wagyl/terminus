package objects

import changes._
import data._

/*
 * Trait des objets pouvant en contenir d'autres.
 * Qu'il s'agiise d'un contenant statique (coffre, etc) ou d'une créature
 * bénéficiant d'un inventaire (dont le joueur).
 */
trait Container extends Entity {

  private var _contents : List[Content] = List()

  def contents : List[Content] = _contents

  def addContent (e : Content) : Boolean = {
    _contents = e :: _contents
    true
  }

  def removeContent (e : Content) : Boolean = {
    _contents = _contents.filter(x => x != e)
    true
  }

  def contains (content : Content) : Boolean = {
    _contents.contains(content)
  }
}

object Container {
  def contains (container : Entity, content : Entity) : Boolean = {
    (container, content) match {
      case (container : Container, content : Content) =>
      if ( container.contains(content) 
	&& content.position == InContainer(container))
	true
      else {
	// INCOHERENCE !
	//println("ATTENTION : incohérence entre la position du contenant et le contenu du container !")
	false
      }
      case _ => false
    }
  }
}


/*
 * Trait des objets pouvant être contenu par d'autres.
 * Ne contient aucune fonctionnalité, mais assure seulement par le typage
 * que n'importe quel objet ne peut être placé dans un Container.
 */
trait Content extends Entity { }


/*
 * Un objet qu'il est possible de ramasser ou de déposer.
 */
trait Pickable extends Content with Usable with Edible { 

  val pickUp = new PickUpAction(this)
  val drop = new DropAction(this)
  
  addUse(pickUp)
  addUsage(drop)

  override def isStackable = true
  
}

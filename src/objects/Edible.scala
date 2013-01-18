package objects

import changes._
import representation._

/*
 * Objets pouvant être utilisés depuis l'inventaire.
 * (Ce qui ne signifie pas nécessairement la disparition de l'objet.)
 */


trait Edible extends Entity {

  private final var usages : List[Action] = List()

  final def addUsage (a : Action) : Unit = {
    usages = a :: usages
  }

  final def getUsages () : List[Action] = {
    return usages
  }

  // TODO : GESTION DE L'ERREUR si l'index n'est pas dans la liste
  final def getUsage (index : Int) : Action = {
    usages(index)
  }

  override def representation(observer : Entity) : Option[EntityInterface] = {
    val rep = super.representation(observer)
    rep match {
      case Some(content : ContentInterface) =>
      val actions = Representation.get(observer, usages)
      content.usages = actions ::: content.usages
      case _ => ;
      }
    rep
  }
}

package objects

import changes._
import representation._

/*
 * Entity capables d'agir par elles-mêmes :
 * elles ont donc à disposition des actions qu'elles peuvent accomplir.
 */

trait Able extends Entity {

  /*
   * Différence de temps de l'Entity par rapport au joueur.
   */
  var diffTime : Double = 0

  final def addTime(dt : Double) {
    diffTime += dt
  }

  final def spendTime(dt : Double) {
    diffTime -= dt
  }


  /*
   * Liste des Actions possibles.
   */
  protected final var abilities : List[Action] = List(new SkipAction)

  final def addAbility (a : Action) : Unit = {
    abilities = a :: abilities
  }

  final def addAbilities(l : List[Action]) : Unit = {
    abilities = l ::: abilities
  }

  final def removeAbility (id : Int) : Unit = {
    abilities = abilities.filterNot(x => (x.identifier == id))
  }

  final def getAbilities () : List[Action] = {
    return abilities
  }

  // TODO : GESTION DE L'ERREUR si l'index n'est pas dans la liste
  final def getAbility (index : Int) : Action = {
    abilities(index)
  }

  final def ability (identifier : Int) : Option[Action] = {
    abilities.find(a => a.identifier == identifier)
  }

  final def contains (a : Action) : Boolean = {
    abilities.contains(a)
  }

  override def representation(observer : Entity) : Option[EntityInterface] = {
    val rep = super.representation(observer)
    rep match {
      case Some(self:SelfInterface) =>
      val actions = Representation.get(observer, abilities)
      self.abilities = actions ::: self.abilities
      case _ => ;
    }
    rep
  }
}



/*
 * Trait des Entity capable de déplacements
 */
trait Moving extends Entity with Able {
  
  /*
   * Vitesse de déplacement de l'objet sur un case "normale".
   */
  var speed : Double = 1

  addAbilities(MoveAction.getNaturalMove(this))
}

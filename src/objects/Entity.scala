package objects

import representation._
import changes._
import data._
import log._

/*
 * Classe abstraite des objets du jeu.
 */

abstract class Entity extends Logable {
  val name : String
  /* Rapide description de l'objet à destination du joueur. */
  def description : String
  /* Identifiant du type d'objet. */
  def identifier : Int

  /* Log par défaut */
  def log = "ENTITY"




  /*
   * Référence sur Game :
   * ATTENTION, doit obligatoirement être non nulle dans le jeu...
   */
  var game : Game = null

  /* Position de l'objet */
  var position : Location = Nowhere()


  
  /*
   * Renvoie la distance entre deux Entity.
   * Retourne une valeur négative si leurs positions ne permet pas de calculer
   * une distance cohérente (i.e., si elles ne sont pas sur la même carte.)
   */
  def distance (other : Entity) : Double = this.distance(other.position)

  def distance (location : Location) : Double = 
    (this.position, location) match {
      case (Coordinates(x1, y1, z1), Coordinates(x2, y2, z2)) =>
      if (z1 == z2)
	scala.math.sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))
      else
	-1
      case _ => -1
    }




  /* Définit si l'objet est visible pour le personnage. */
  def isVisible = true

  /* Définit si l'objet bloque la vue du personnage (ou s'il voit derrière). */
  def isTransparent = true

  /* Définit si le personnage peut aller sur la case que l'objet occupe.
   * Plus généralement, deux objets non stackables ne peuvent être sur la même
   * case.
   * */
  def isStackable = false




  /*
   * Représentation de l'objet à destinattion d'un autre objet
   * (à priori, le joueur).
   */
  def representation (observer : Entity) : Option[EntityInterface] = {
    val that = this
      // si l'observer est précisément l'entity :
    if (this == observer)
      Some ( new SelfInterface (name, identifier) {
	val position = that.position match {
	  case c : Coordinates => c
	  case _ => null
	}
	var description = that.description
      } )
    // si l'observer contient l'entity :
    else if (Container.contains(observer, this))
      Some ( new ContentInterface (name, identifier) {
	var  description = that.description
      } )
    // si l'entity est sur la carte :
    else if (that.position.isInstanceOf[Coordinates]) {
      val rep = new ObjectInterface (name, identifier) {
	val position = that.position.asInstanceOf[Coordinates]
	var description = that.description
      }
      Some(rep)
    }
    // sinon :
     else
       None
  }


  /*
   * Système de notification des Changes sur les objets
   * et de modification de ces Changes par les objets.
   */

  private final var filters : List[Filter] = List()

  final def addFilter (f : Filter) = { filters = f :: filters }


  final def modify (e : Effect) : Effect => Effect = {
    /*
    val funs : List[Change => Change] = filters.map(x => Filter.fun(c, x))
    val fun : Change => Change = funs.foldLeft(Filter.identity)(Filter.compose)
    fun
    */
    (e => e)
  }
}


object Entity {
  val god = new Blank
  val blank = Blank.get
}


/*
 * La classe abstraite des entités qui agissent d'elles-mêmes à chaque tour.
 */
abstract class MutableEntity extends Entity {
  
  def mutation() : Unit

}

/*
 * La classe asbtraite des Entity constantes, par opposition aux MutableEntity.
 */
abstract class ConstEntity extends Entity {

}


/*
 * Un objet spécial "blanc".
 */
class Blank extends Entity
 {
   val name = "NULL"
   val description = ""
   val identifier = 0
}

object Blank {
  val b = new Blank
  def get = b
}

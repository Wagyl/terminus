package objects

import changes._

/*
 * FICHIER EN CONSTRUCTION
 * Ressources : mana, rage, énergie, etc.
 * Les points de vie sont traités à part : cf Life.scala.
 */


/*
 * Trait des Entity disposant d'un Set de Ressources.
 */
trait Resources extends Entity {
  var resources : ResourcesSet = new ResourcesSet
}


/*
 * Ensemble de ressources.
 */
class ResourcesSet {
  var resources : List[Resource] = List()

  def addResource (r : Resource) {
    resources = r :: resources
  }

  // TODO : suppression d'une ressource
}


trait Mana extends ResourcesSet {
  val mana : Resource = new ResourceMana

  addResource (mana)
}


/*
 * La classe abstraite d'une ressource.
 */
abstract class Resource {
  val name : String
  var value = 0
  var maxValue = 0

  def add (diff : Int) : Unit = {
    value = scala.math.min(maxValue, value + diff)
  }

  def sub (diff : Int) : Unit = {
    value = scala.math.min(0, value - diff)
  }

  // TODO : modificateurs du max.
}

/*
 * Une ressource concrète : la mana.
 */
class ResourceMana extends Resource {
  val name = "mana"
}

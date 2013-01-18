package objects

import representation._
import combat._
import scala.collection.mutable.HashMap
import changes.CanControl

object Character {
  def apply(classId: Int, name: String, descr: String): Character = {
    classId match {
      case Identifier.warrior ⇒ new Warrior(name, descr) with CanControl
      case Identifier.sorcerer ⇒ new Sorcerer(name, descr) with CanControl
      case Identifier.rogue ⇒ new Rogue(name, descr) with CanControl
      case _ ⇒ new Warrior(name, descr) with CanControl
    }
  }
}

abstract class Character(
  val name: String,
  val description: String,
  val baseHealth: Float)
  extends Creature with Humanoid with Memory with ConcreteInits {

  addCoterie(Adventurer ())
  addRelation((Monster (), Enemy ()))
    
  active = false

  def eyesight = 5
    
  def classIdentifier: Int
  val identifier = Identifier.character
  val defaultSpell = CompSpell

  //TODO representation des stats
  override def initRepresentation() = {
    val that = this
    val cls = Character(that.classIdentifier, "", "")
    val clsI = new ClassInterface(that.name, that.baseHealth, null) {
      var description = that.description
      def identifier = that.classIdentifier
    }
    new SavedCharacterInterface(that.name) {
      val classInf = clsI
      val gender = 0
      val stats = null
      var description = that.description
    }
  }
}

object Player {
  def apply(name: String, descr: String): Player = {
    new ConcretePlayer(name, descr)
  }
}

abstract class Player(
  val name: String,
  val description: String) extends ConcreteInits {
  var nbChars = -1
  val characters = new HashMap[Int, Character]
  def addChar(char: Character) {
    nbChars += 1
    characters.put(nbChars, char)
  }
  def getChars(): List[(Int, Character)] = {
    var list = List[(Int, Character)]()
    for ((i, c) ← characters) {
      list = (i, c) :: list
    }
    list
  }

  override def initRepresentation() = {
    val that = this
    new ProfileInterface(that.name, that.nbChars) {
      var description = that.description
    }
  }
}

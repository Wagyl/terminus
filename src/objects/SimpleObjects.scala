package objects

import changes._
import combat._
import representation._

class Steak(hp: Int) extends ConstEntity with Eatable {
  val name = "steak"
  val description = "Un steak."
  val identifier = 11
  var diffHp = hp
  override def isStackable = true
  override def isTransparent = true
}


class Door extends ConstEntity with Openable {
  lazy val name = "Door"
  val description = "Une porte"
  def identifier = if (isOpen) Identifier.doorOpen else Identifier.doorClosed
}


class Statue extends ConstEntity with Usable {
  val name = "Statue"
  val description = "Une statue..."
  val identifier = Identifier.statue

  addUse(new AdmireAction(this))

  override def isTransparent = false
}


class Altar extends ConstEntity with Usable {
  val name = "Altar"
  val description = "Un autel..."
  val identifier = Identifier.altar

  addUse(new PrayAction(this))

  override def isTransparent = false
}

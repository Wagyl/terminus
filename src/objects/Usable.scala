package objects

import changes._
import representation._
import combat._

/*
 * Entity pouvant être utilisés par d'autres Entity Able
 * lorsqu'elles sont sur le terrain.
 */

trait Usable extends Entity {
  private final var uses: List[Action] = List()

  final def addUse(a: Action): Unit = {
    uses = a :: uses
  }

  final def getUses(): List[Action] = {
    return uses
  }

  // TODO : GESTION DE L'ERREUR si l'index n'est pas dans la liste
  final def getUse(index: Int): Action = {
    uses(index)
  }

  final def useByIdent (identifier : Int) : Option[Action] = {
    uses.find(a => a.identifier == identifier)
  }

  def getDefaultUse (a : Entity) : Option[Action] = None

  override def representation(observer: Entity): Option[EntityInterface] = {
    val rep = super.representation(observer)
    rep match {
      case Some(obj: ObjectInterface) =>
        val actions = Representation.get(observer, uses)
        obj.uses = actions ::: obj.uses
      case _ => ;
    }
    rep
  }
}

trait Openable extends Entity with Usable {
  private final var _open: Boolean = false

  val openAction: OpenAction = new OpenAction(this)
  val closeAction: CloseAction = new CloseAction(this)

  addUse(closeAction)
  addUse(openAction)

  override def getDefaultUse (other : Entity) : Option[Action] = {
    if (!isOpen)
      Some(openAction)
    else
      super.getDefaultUse(other)
  }

  final def isOpen: Boolean = _open

  final def open(): Boolean = {
    val s = _open
    _open = true
    !s
  }

  final def close(): Boolean = {
    val s = _open
    _open = false
    s
  }

  override def isStackable = _open
  override def isTransparent = _open
}


trait Eatable extends Entity {
  var diffHp: Int
}

trait Victimizable extends Usable with Life with StatHandler
with RelationShip {
  
  val castAction = new CastAction(this)
  addUse(castAction)
  
  override def getDefaultUse (a : Entity) : Option[Action] = 
    if (this.relation(a) == Enemy ())
      Some(castAction)
    else
      super.getDefaultUse(a)
}

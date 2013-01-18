package changes

import representation._
import objects._

/**
 * Prise de contrôle d'une entité.
 */
class TakeControlAction(val controlled: Creature with Controlable) extends Action {
  val name = "Take control"
  def description = "Prendre le contrôle de " + controlled.name
  val identifier = Identifier.controlAction
  var controller: Option[Creature with CanControl] = null

  addEffect(new Effect {
    val change = new TakeControlChange
    def effector = controller.get
    val target = controlled
  })

  override def time(a: Entity): Double = 0

  override def assign(a: Entity): (Boolean, String) = {
    a match {
      case x: Creature with CanControl ⇒ controller = Some(x)
      case _ ⇒ return (false, "Vous ne pouvez pas prendre le contrôle d'une entité.")
    }

    (true, "")
  }

  override def abort() = {
    controller = None
  }
}

class TakeControlChange extends Change {
  val name = "control"

  type Effector = Creature with CanControl
  type Target = Creature with Controlable

  override def possible(effector: Effector, target: Target) = {
    super.possible(effector, target)
  }

  /**
   * Définit la cible comme étant le personnage actif,
   * lui ajoute la possibilité de revenir dans son corps,
   * active/désactive les IA.
   */
  override def apply(effector: Effector, target: Target): (Outcome, Double) = {
    target.game.setCharacter(target)
    target.addAbility(new UndoControlAction(effector, target))
    target.active = false
    effector.active = true
    (Outcome(true, "Vous avez pris le contrôle de l'entité."), 0)
  }
}

/**
 * Retourner dans son corps d'origine.
 */
class UndoControlAction(controller: Creature with CanControl, controlled: Creature with Controlable) extends Action {
  val name = "Undo take control"
  def description = "Retourner dans son corps d'origine."
  val identifier = Identifier.controlAction

  addEffect(new Effect {
    val change = new UndoTakeControlChange
    def effector = controlled
    val target = controller
  })

  override def time(a: Entity): Double = 0

  override def assign(a: Entity): (Boolean, String) = {
    (true, "")
  }

  override def abort() = {
  }
}

class UndoTakeControlChange extends Change {
  val name = "undo control"

  type Effector = Creature with Controlable
  type Target = Creature with CanControl

  override def possible(effector: Effector, target: Target) = {
    super.possible(effector, target)
  }

  override def apply(effector: Effector, target: Target): (Outcome, Double) = {
    target.game.setCharacter(target)
    effector.removeAbility(Identifier.undoControlAction)
    target.active = false
    effector.active = true
    (Outcome(true, "Vous êtes retourné dans votre corps."), 0)
  }
}

/**
 * Les traits à donner aux créatures qui peuvent contrôler ou être contôlées.
 */
trait Controlable extends Creature {
  val controlAction: TakeControlAction = new TakeControlAction(this)
  addUse(controlAction)
}

trait CanControl extends Creature {}
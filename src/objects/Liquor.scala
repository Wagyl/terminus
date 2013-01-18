package objects

import changes._



trait Potable extends Usable {
  
  type Liquor = List[Change {type Effector >: Creature <: Entity; type Target = Entity}]

  val drinkPotableAction = new DrinkPotableAction(this)
  addUse(drinkPotableAction)

  val transferLiquor = new TransferLiquorAction(this)
  addUse(transferLiquor)

  override def getDefaultUse (other : Entity) : Option[Action] = 
    Some(drinkPotableAction)

  def liquor : Liquor
  def drinkMessage : String
}


trait Waterproof extends Edible {
  
  type Liquor = List[Change {type Effector >: Creature <: Entity; type Target = Entity}]

  val drinkAction = new DrinkAction(this)
  addUsage(drinkAction)

  protected var _liquor : Liquor = List()
  protected var _drinkMessage : String = ""

  def liquor : Liquor = _liquor
  def drinkMessage : String = _drinkMessage

  protected var dose = 0
  protected val maxDose = 1
  
  def isEmpty : Boolean = { dose == 0 }
  def isFull : Boolean = { dose == maxDose }

  def fill (l : Liquor, message : String) : Boolean = {
    if (dose < maxDose)
      dose = maxDose
    _liquor = l
    _drinkMessage = message
    true
  }

  def empty : Boolean = { 
    dose = 0
    false
  }

  def drink : Boolean = {
    if (dose > 0) {
      dose -= 1
      true
    }
    else
      false
  }
}

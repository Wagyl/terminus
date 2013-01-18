package gui.unnamedterface
import representation.EntityInterface
import representation.ActionInterface
import representation.AbleInterface
import representation.EdibleInterface
import representation.UsableInterface

object UnnamedBuilder {

  /* Construire une UnnamedAction */
  def buildUnnamedAction(buildFrom: ActionInterface, tmpId: Int): UnnamedAction =
    new UnnamedAction {
      val lastTmpId = tmpId
      val id = buildFrom.identifier
      val name = buildFrom.name
      val description = buildFrom.description
    }

  /* Construction auxiliaire d'une liste d'UnnamedAction */
  def buildActionListAux(data: List[(Int, ActionInterface)]): List[UnnamedAction] =
    data.map(dt => buildUnnamedAction(dt._2, dt._1))

  /* Construction d'une liste d'UnnamedAction */
  def buildActionList(buildFrom: EntityInterface): Option[List[UnnamedAction]] = {
    buildFrom match {
      case x: AbleInterface => buildActionListAux(x.abilities)
      case x: EdibleInterface => buildActionListAux(x.usages)
      case x: UsableInterface => buildActionListAux(x.uses)
      case _ => None
    }
    None
  }

  /* Construire une UnnamedEntity */
  def buildUnnamedEntity(buildFrom: EntityInterface, tmpId: Int): UnnamedEntity =
    new UnnamedEntity {
      val lastTmpId = tmpId
      val id = buildFrom.identifier
      val name = buildFrom.name
      val description = buildFrom.description
      val actions = buildActionList(buildFrom)
      val image = ImageCache.getImage(id)
    }

  /* Construire une UnnamedTile */
  def buildUnnamedTile(floor: Int): UnnamedTile =
    new UnnamedTile { val floorId = floor }
}
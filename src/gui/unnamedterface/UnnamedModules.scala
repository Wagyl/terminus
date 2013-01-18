package gui.unnamedterface

import representation.OnMap
import representation.EntityInterface
import representation.ActionInterface
import representation.ContentInterface
import representation.TileInterface

/* Traits des modules "préfaits" */

/* Affichage de la zone de jeu autour du personnage */
trait Display extends ObjAndMapsModule {
  /* Dessine le sol */
  def drawFirstLayer(abs: Int, ord: Int, toDraw: Option[UnnamedEntity]): Unit

  /* Dessine les objets autre que le sol */
  def drawObjects(abs: Int, ord: Int, toDraw: List[UnnamedEntity]): Unit

  /* Méthode du trait ObjAndMapsModule */
  override def refresh(abs: Int, ord: Int, data: UnnamedTile): Unit = {
    drawFirstLayer(abs, ord, data.firstLayer)
    drawObjects(abs, ord, data.content)
  }
}

/* Affichage des actions disponnibles sur la case ciblée*/
trait ActionDisplay extends TargetDependentModule {
  def displayAction(toDisplay: UnnamedAction): Unit

  def clearActions(): Unit

  override def refresh(data: Option[UnnamedTile]): Unit =
    data match {
      case Some(x) =>
        for (c <- x.content)
          c.actions match {
            case Some(l) => for (act <- l) displayAction(act)
            case None => ()
          }
      case None => clearActions()
    }
}

/* Affichage de la carte de l'étage en cours */
trait MapDisplay extends MapsModule {
  def addTile(abs: Int, ord: Int, toDraw: UnnamedEntity): Unit

  def clearTile(abs: Int, ord: Int): Unit

  override def refresh(abs: Int, ord: Int, data: Option[UnnamedEntity]): Unit =
    data match {
      case Some(t) => addTile(abs, ord, t)
      case None => clearTile(abs, ord)
    }
}

/* Affichage de l'inventaire */
trait InventoryDisplay extends InventoryModule {
  def displayInventory(toDisplay: List[UnnamedEntity]): Unit

  override def refresh(data: List[UnnamedEntity]): Unit =
    displayInventory(data)
}

/* UnitFrame */
trait UnitFrame extends CharacterModule {
  def displayCharacter(toDisplay: UnnamedEntity): Unit

  override def refresh(data: UnnamedEntity): Unit =
    displayCharacter(data)
}
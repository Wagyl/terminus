package gui.unnamedterface

import controller.GameControl
import representation.ActionInterface
import representation.ContentInterface
import representation.LandInterface
import representation.EntityInterface
import data.Coordinates
import representation.OnMap
import controller.Out
import representation.SelfInterface
import representation.TileInterface

/* Trait module général */
abstract trait UnnamedModule

/* Module ayant besoin d'une référence au handler */
trait HandlerDependentModule extends UnnamedModule {
  val handler: UnnamedDisplayHandler
}

/* Module ayant besoin du joueur */
trait CharacterModule extends UnnamedModule {
  def refresh(data: UnnamedEntity): Unit
}

/* Module ayant besoin des objets sur la carte */
trait ObjectsModule extends UnnamedModule {
  def refresh(abs: Int, ord: Int, data: UnnamedEntity): Unit
}

/* Module ayant besoin des "sols" */
trait MapsModule extends UnnamedModule {
  def refresh(abs: Int, ord: Int, data: Option[UnnamedEntity]): Unit
}

/* Module ayant besoin des cases entières */
trait ObjAndMapsModule extends UnnamedModule {
  def refresh(abs: Int, ord: Int, data: UnnamedTile): Unit
}

/* Module ayant besoin de l'inventaire */
trait InventoryModule extends UnnamedModule {
  def refresh(data: List[UnnamedEntity]): Unit
}

/* Module travaillant sur la case ciblée */
trait TargetDependentModule extends UnnamedModule {
  def refresh(data: Option[UnnamedTile]): Unit
}

/* Handler */
abstract class UnnamedDisplayHandler extends Out {
  val gameControl: GameControl
  val dataTree: UnnamedData

  /* Les listes de souscription */
  var characterModules: List[CharacterModule] = List()
  var objectsModules: List[ObjectsModule] = List()
  var mapsModules: List[MapsModule] = List()
  var inventoryModules: List[InventoryModule] = List()
  var objAndMapsModules: List[ObjAndMapsModule] = List()
  var targetDependentModules: List[TargetDependentModule] = List()

  /* Variable de joueur */
  var playerCoord: Option[Coordinates] = None

  /* Variable de cible */
  var targetedCoord: Option[Coordinates] = None

  /* Souscription des modules aux différentes entrées */
  def registerModule(toRegister: UnnamedModule) {
    toRegister match {
      case mod: CharacterModule =>
        characterModules = mod :: characterModules
      case mod: ObjectsModule =>
        objectsModules = mod :: objectsModules
      case mod: MapsModule =>
        mapsModules = mod :: mapsModules
      case mod: InventoryModule =>
        inventoryModules = mod :: inventoryModules
      case mod: ObjAndMapsModule =>
        objAndMapsModules = mod :: objAndMapsModules
      case mod: TargetDependentModule =>
        targetDependentModules = mod :: targetDependentModules
    }
  }

  /* Faire une action */
  def action: Unit = ()

  /* Récupérer le joueur */
  def playerEntity(): Option[UnnamedEntity]

  /* Récupérer la case ciblée */
  def targetedTile(): Option[UnnamedTile] = targetedCoord match {
    case Some(coord) => dataTree.getFloor(coord.z.z).getTile(coord)
    case None => None
  }

  /* Sélectionner une case */
  def targetTile(toTarget: Coordinates): Unit = {
    targetedCoord = Some(toTarget)
    targetModuleRefresh()
  }

  /* Déselectionner la target */
  def untargetTile(): Unit = {
    targetedCoord = None
    targetModuleRefresh()
  }

  /* Refresh modules target-dépendant */
  def targetModuleRefresh(): Unit =
    for (mod <- targetDependentModules) mod.refresh(targetedTile())

  /* Méthodes du trait Out */
  /* Le personnage */
  override def character(data: (Int, SelfInterface)): Unit =
    for (mod <- characterModules)
      mod.refresh(UnnamedBuilder.buildUnnamedEntity(data._2, data._1))

  /* Tous les sols / objets case par case */
  override def allObjects(data: List[((Int, TileInterface), List[(Int, EntityInterface)])]): Unit = {
    var added: (Int, Int, UnnamedEntity) = null

    for (dt <- data) {
      /* Mise à jour des données de l'IG */
      val tmpCoord: Coordinates = dt._1._2.position
      val tmpTile: UnnamedTile = dataTree.getFloor(tmpCoord.z.z).addVisibleTile(dt._1._2.position, dt)

      for (mod <- objAndMapsModules) mod.refresh(tmpCoord.x, tmpCoord.y, tmpTile)
      for (mod <- mapsModules) mod.refresh(tmpCoord.x, tmpCoord.y, tmpTile.firstLayer)
      if (!objectsModules.isEmpty)
        for (obj <- tmpTile.content)
          for (mod <- objectsModules) mod.refresh(tmpCoord.x, tmpCoord.y, obj)
    }

    /* Il y a moyen de faire mieux que de refresh les modules cible-dépendants
     * à chaque fois, à voir ! */
    targetModuleRefresh()
  }

  /* Le contenu de l'inventaire */
  override def inventory(data: List[(Int, ContentInterface)]): Unit =
    for (mod <- inventoryModules)
      mod.refresh(data.map(dt => UnnamedBuilder.buildUnnamedEntity(dt._2, dt._1)))
}
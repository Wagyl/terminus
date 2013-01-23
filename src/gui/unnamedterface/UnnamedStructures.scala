package gui.unnamedterface

import java.awt.image.BufferedImage
import data.Coordinates
import scala.collection.mutable.HashMap
import representation.ObjectInterface
import data.Land
import representation.EntityInterface
import representation.TileInterface
import representation.LandInterface
import representation.OnMap
import representation.SelfInterface

/* A besoin d'un accès au builder */
trait NeedBuilder {
  val builder: UnnamedBuilder
}

/* Data de base */
abstract class UnnamedBaseStruct {
  val lastTmpId: Int
  val id: Int
  val name: String
  val description: String
}

/* Data pour une action */
abstract class UnnamedAction extends UnnamedBaseStruct

/* Data pour une entité */
abstract class UnnamedEntity extends UnnamedBaseStruct {
  val actions: Option[List[UnnamedAction]]
  val image: BufferedImage
}

/* Data pour une case */
abstract class UnnamedTile extends NeedBuilder {
  var firstLayer: Option[UnnamedEntity] = None
  var content: List[UnnamedEntity] = List()

  /* Il est pratique d'avoir un accès rapide à l'id de l'étage pour chaque case */
  val floorId: Int

  /* Ajout d'une entité : se charge de différencier un sol d'un objet autre */
  def addEntity(toAdd: EntityInterface, tmpId: Int): UnnamedEntity = {

    val unnamedEntityToAdd = builder.buildUnnamedEntity(toAdd, tmpId)

    toAdd match {
      case x: TileInterface => majFirstLayer(unnamedEntityToAdd)
      case _ => addUnnamedEntity(unnamedEntityToAdd)
    }
    unnamedEntityToAdd
  }

  /* Ajout d'un objet autre qu'un sol */
  def addUnnamedEntity(toAdd: UnnamedEntity): Unit = toAdd :: content

  /* Ajout d'un sol */
  def majFirstLayer(toAdd: UnnamedEntity): Unit = firstLayer = Some(toAdd)
}

/* Data pour un étage */
abstract class UnnamedDataFloor extends NeedBuilder {
  var visibleDataFloor: HashMap[Coordinates, UnnamedTile] =
    new HashMap[Coordinates, UnnamedTile]

  /* Ajout de data à une coordonnée */
  def addVisibleObject(toAdd: EntityInterface with OnMap,
    tmpId: Int): (Int, Int, UnnamedEntity) = {

    val tmpCoord: Coordinates = toAdd.position
    var result: UnnamedEntity = null

    visibleDataFloor.get(tmpCoord) match {
      case Some(e) => result = e.addEntity(toAdd, tmpId)
      case None => {
        val tmp: UnnamedTile = builder.buildUnnamedTile(tmpCoord.z.z)
        result = tmp.addEntity(toAdd, tmpId)
        visibleDataFloor += (tmpCoord -> tmp)
      }
    }
    (tmpCoord.x, tmpCoord.y, result)
  }

  /* Ajout/Modification d'une case entière */
  def addVisibleTile(coord: Coordinates,
    toAdd: ((Int, TileInterface), List[(Int, EntityInterface)])): UnnamedTile = {
    val tmp: UnnamedTile = builder.buildUnnamedTile(coord.z.z)
    tmp.addEntity(toAdd._1._2, toAdd._1._1)
    for (data <- toAdd._2)
      tmp.addEntity(data._2, data._1)

    visibleDataFloor += (coord -> tmp)
    tmp
  }

  /* Récupérer les entités d'une case */
  def getTile(coord: Coordinates): Option[UnnamedTile] =
    visibleDataFloor.get(coord)
}

/* Data pour toute la carte */
class UnnamedData {
  var dataMap: HashMap[Int, UnnamedDataFloor] =
    new HashMap[Int, UnnamedDataFloor]

  /* Récupérer les données d'un étage */
  def getFloor(map: LandInterface): UnnamedDataFloor = {
    val floorId: Int = map.floor.z
    getFloor(floorId)
  }

  def getFloor(floorId: Int): UnnamedDataFloor = {
    dataMap.get(floorId) match {
      case Some(x) => x
      case None => {
        val tmp: UnnamedDataFloor =
          new UnnamedDataFloor { val builder: UnnamedBuilder = this.builder }
        dataMap += (floorId -> tmp)
        tmp
      }
    }
  }
}

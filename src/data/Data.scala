package data

import objects._

/*
 * La classe abstraite des données.
 * Contient toutes les Entity non Tile présent sur la carte.
 */

abstract class Data (val game : Game) {

  /*
   * Renvoie la liste de toutes les entités d'une position donnée
   * sur une carte donnée.
   */
  def getEntitiesAt (c : Coordinates) : List[Entity]

  /*
   * Renvoie la liste des entités mutables d'une carte donnée.
   */
  def getMutableEntities (m : Floor) : List[MutableEntity]

  /*
   * Renvoie la liste des entités non mutables d'une position donnée
   * sur une carte donnée.
   */
  def getConstEntitiesAt (c : Coordinates) : List[ConstEntity]


  /*
   * Ajout d'un objet dans Data.
   * Valide pour une location Coordinates et un ConstEntity ou MutableEntity
   * ou une location InContainer et un Content.
   * Remplit correctement les champs game et position de [obj].
   */
  def addEntityAt (obj : Entity, pos : Location) : Boolean

  /*
   * Suppression d'un objet.
   * Renvoie true si la méthode ne rencontre aucune difficulté, y compris
   * si [obj] n'est pas trouvé dans Data.
   * Place la position de [obj] à Removed.
   */
  def removeEntity (obj : Entity) : Boolean


  /*
   * Déplacement d'un objet.
   * Aucune condition n'est vérifiée sur l'objet déplacé : s'il peut être
   * placé à la location souhaitée, le déplacement se fera.
   */
  def moveEntityAt(obj : Entity, pos : Location) : Boolean


  /* fonctions utilitaires */

  def getAround (c : Coordinates, radius : Int) : List[Entity] = {
    var rv = List[Entity]()
    for (i <- c.x - radius to c.x + radius)
      for (j <- c.y - radius to c.y + radius)
	if (i != c.x || j != c.y)
	  rv :::= getEntitiesAt(Coordinates(i, j, c.z))
    rv	  
  }

}



/*
 * Création d'un Data concret.
 */
object Data {

  def create (game : Game) = {
    new ActualData(game)
  }

}

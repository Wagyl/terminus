package controller

import changes._
import representation._
import objects._
import data._
import scala.collection.mutable.HashMap
import communication.DistantController

/**
 * Usine qui crée le game controller par défaut ou en fonction des extensions désirées.
 */
object GameControl {
  def apply(char: Character, out: Out): GameControl = new LocalGameControl(char, out)

  def apply(char: Character, out: Out, extensionsIds: List[Int]): GameControl = {
    if (extensionsIds.contains(Identifier.realTime) && extensionsIds.contains(Identifier.hotSeat)) {
      new LocalGameControl(char, out) with HotSeatControl with RealTime { override val time = 1; override val sleep = 2000 }
    } else if (extensionsIds.contains(Identifier.realTime)) {
      new LocalGameControl(char, out) with RealTime { override val time = 1; override val sleep = 2000 }
    } else if (extensionsIds.contains(Identifier.hotSeat)) {
      new LocalGameControl(char, out) with HotSeatControl
    } else if (extensionsIds.contains(Identifier.client)) {
      new DistantController("localhost", 8642, out) //TODO IP en paramètre
    } else {
      this(char, out)
    }
  }
}

/** La classe que l'IG doit voir. */
abstract class GameControl extends In {}

/** Et son implémentation concrète. */
class LocalGameControl(val char: Character, protected val out: Out) extends GameControl {
  /** Toutes les données du jeu. */
  protected lazy val game: Game = Game.create()

  /** La "couche contrôleur" par dessus ces données. */
  protected val representation = new ControlRepresentation()

  /* Initialisation des données. */
  game.world.addLand(Default(Infinite()), "Transitivité Conceptuelle", "Seule la transitivité conceptuelle vous permettra d'atteindre la complétude véritable.")
  game.setCharacterAt(char, Coordinates(4, 4, Floor(0)))
  turn(0)

  /**** IN ****/

  override def make(actionId: Int): Result = synchronized {
    var action: Action = game.character.getAbility(actionId)

    /*
     * Si l'action est une action de déplacement, et qu'il cible en réalité
     * une case occupée par un objet non stackable Usable qui définit une
     * action par défaut, alors accomplir cette action.
     */
    if (action.isInstanceOf[MoveAction]) {
      action.asInstanceOf[MoveAction].target(game.character) match {
        case Some(e: Usable) ⇒
          e.getDefaultUse(game.character) match {
            case Some(a) ⇒ action = a
            case None ⇒ ;
          }
        case Some(_) ⇒ ;
        case None ⇒ ;
      }
    }
    actionAux(action)
    //TODO voir avec Action ce qu'il se passe quand l'action n'existe pas?
  }

  override def use(entityId: Int, useId: Int): Result = synchronized {
    val rc = representation.usableEntities.get(entityId)
    var entity: Entity with Usable = null
    rc match {
      case None ⇒ return Error("Entité introuvable (ou non Usable)")
      case Some(e) ⇒ entity = e
    }
    val action: Action = entity.getUse(useId)
    actionAux(action)
  }

  override def consume(entityId: Int, edibleId: Int): Result = synchronized {
    val rc = representation.edibleEntities.get(entityId)
    var entity: Entity with Edible = null
    rc match {
      case None ⇒ return Error("Entité introuvable (ou non Edible)")
      case Some(e) ⇒ entity = e
    }
    val action: Action = entity.getUsage(edibleId)
    actionAux(action)
  }

  private def actionAux(action: Action): Result = {
    var rc: Result = null
    if (!action.possible(game.character)) rc = Impossible("Action impossible à réaliser.")
    else rc = action.perform(game.character)
    turn(-game.character.diffTime)
    game.character.diffTime = 0
    rc
  }

  /**** OUT ****/

  /**
   * Envoi des données à l'interface graphique.
   */
  protected def toOut() = {
    out.inventory(inventory())
    out.maps(maps())
    out.objects(objects())
    out.character(character())
    out.allObjects(allObjects())

    // Envoi des évènements passés
    this.game.character match {
      case c: Memory =>
        val notifications = c.getNotices
        val events = notifications.map(_.toEvent())
        events foreach {
          out.event(_)
        }
      case _ => ()
    }
  }

  /*** Fonctions auxiliaires pour Out ***/

  /**
   * Renvoie l'identifiant temporaire du joueur.
   */
  private def getCharacterId(): Int = {
    for (obj ← objects()) {
      obj match {
        case (tmpId, tmpInterf: SelfInterface) ⇒ return tmpId
        case _ ⇒ ;
      }
    }
    return 0
  }

  private def character(): (Int, SelfInterface) = synchronized {
    val cr = game.character.representation(game.character)
    var id = getCharacterId()

    cr match {
      case None ⇒ throw new ControllerException("Personnage introuvable")
      case Some(c) ⇒
        if (c.isInstanceOf[SelfInterface]) (id, c.asInstanceOf[SelfInterface])
        else throw new ControllerException("Personnage introuvable")
    }
  }

  private def objects(): List[(Int, EntityInterface)] = synchronized {
    var list = List[(Int, EntityInterface)]()

    /* Objets visibles par le joueur */
    for ((i, e) ← representation.visibleEntities) {
      val er = e.representation(game.character)
      er match {
        case None ⇒ ()
        case Some(ei) ⇒ list = (i, ei) :: list
      }
    }

    list
  }

  private def allObjects(): (List[((Int, TileInterface), List[(Int, EntityInterface)])]) = synchronized {
    var list = List[((Int, TileInterface), List[(Int, EntityInterface)])]()

    /* Cases visibles par le joueur et entités corespondantes */
    for ((i, e) ← representation.visibleTiles) {
      val er = e.representation(game.character)
      er match {
        case None ⇒ ()
        case Some(ei: TileInterface) ⇒
          val entities = representation.entitiesForTiles.get(i)
          var entitiesRep: List[(Int, EntityInterface)] = Nil
          entities match {
            case None ⇒ ()
            case Some(es) ⇒ entitiesRep = es.map(elm ⇒ (elm._1, elm._2.representation(game.character))).filterNot(_._2.isEmpty).map(elm ⇒ (elm._1, elm._2.get))
          }
          list = ((i, ei), entitiesRep) :: list
        case _ ⇒ ()
      }
    }

    list
  }

  private def maps(): LandInterface = synchronized {
    /* Récupérer la carte */
    val curLand = game.world.land(Floor(0))

    /* Créer la representation de la carte pour le joueur */
    val li = new LandInterface(curLand.name, curLand.floor.z) {
      var description = curLand.description
      val height = curLand.height
      val width = curLand.width
      val floor = curLand.floor
    }

    /* Tiles visibles par le joueur */
    for ((i, e) ← representation.visibleTiles) {
      val er = e.representation(game.character)
      er match {
        case None ⇒ ()
        case Some(ei: TileInterface) ⇒ li.tiles = (i, ei) :: li.tiles
        case _ ⇒ ()
      }
    }
    li
  }

  private def inventory(): List[(Int, ContentInterface)] = synchronized {
    var list = List[(Int, ContentInterface)]();

    /* Objets dans l'inventaire du joueur */
    for ((i, e) ← representation.edibleEntities) {
      val er = e.representation(game.character)
      er match {
        case None ⇒ ()
        case Some(ei) ⇒
          if (ei.isInstanceOf[ContentInterface]) {
            list = (i, ei.asInstanceOf[ContentInterface]) :: list
          }
      }
    }

    list
  }

  //TODO
  override def save(): Unit = {
  }

  override def quit(save: Boolean): Unit = {
  }

  /**** CONTROL ****/

  /**
   * Passage d'un tour : mise à jour des entités mutables,
   * mises à jours des données,
   * envoi des nouvelles données à l'interface graphique.
   */
  def turn(time: Double) = synchronized {
    mutations(time)
    updateData()
    toOut()
  }

  /**
   *  Mise à jour des mutables
   */
  private def mutations(time: Double) = synchronized {
    val mutables = game.data.getMutableEntities(Floor(0))
    for (m ← mutables) {
      m match {
        case x: Entity with Able ⇒ x.addTime(time)
        case _ ⇒ ()
      }
      m.mutation
    }
  }

  /**
   * Mise à jour des données du contrôleur.
   */
  private def updateData() = synchronized {
    representation.usableEntities.clear
    representation.edibleEntities.clear
    representation.visibleEntities.clear
    representation.visibleTiles.clear
    representation.entitiesForTiles.clear

    /* Remplissage des HashMap. */
    val containerEntities = game.character.contents
    var uId = 1
    var tileId = 1

    // Entités et Cases de la carte vues par le joueur.
    val seen = game.character.seen
    for ((tile, entities) ← seen) {
      var entitiesList: List[(Int, Entity)] = Nil
      for (entity ← entities) {
        entitiesList = (uId, entity) :: entitiesList

        entity match {
          case x: Entity with Usable ⇒ representation.usableEntities.put(uId, x)
          case _ ⇒ ()
        }
        representation.visibleEntities.put(uId, entity)
        uId += 1
      }

      representation.entitiesForTiles.put(tileId, entitiesList)
      representation.visibleTiles.put(tileId, tile)
      tileId += 1
    }

    // Entités contenues dans l'inventaire du joueur.
    uId = 1
    for (e ← containerEntities) {
      e match {
        case x: Entity with Edible ⇒ representation.edibleEntities.put(uId, x); uId += 1
        case _ ⇒ ()
      }
    }

  }
}

/**
 * Tout ce qu'il faut au contrôleur pour être heureux dans la vie.
 */
class ControlRepresentation() {
  /**
   * Tous les objets utilisables qui sont sur le terrain.
   */
  val usableEntities = new HashMap[Int, Entity with Usable]()

  /**
   * Tous les objets utilisables qui sont dans l'inventaire.
   */
  val edibleEntities = new HashMap[Int, Entity with Edible]()

  /**
   * Tous les objets auxquels le joueur peut accéder.
   */
  val visibleEntities = new HashMap[Int, Entity]()

  /**
   * Toutes les cases que le joueur peut voir.
   */
  val visibleTiles = new HashMap[Int, Tile]()

  /**
   * Les objets accessible pour chaque case
   */
  val entitiesForTiles = new HashMap[Int, List[(Int, Entity)]]()
}

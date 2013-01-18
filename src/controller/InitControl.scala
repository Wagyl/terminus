package controller;

import representation._
import objects.Character
import scala.collection.mutable.HashMap
import objects.Player
import objects.ConcretePlayer
import communication.DistantController

/**
 * Façade pour l'initialisation d'une partie.
 *
 */
trait Init {
  /**
   * Renvoie un certains nombres de préférences du (des) joueur(s),
   * telles que la langue du jeu.
   */
  def getPreferences(): PrefsInterface

  /**
   * Sauvegarde ces mêmes préférences.
   */
  def setPreferences(pref: PrefsInterface): Unit

  /**
   * Renvoie les différents profils (de joueurs) existants.
   */
  def getProfiles(): List[(Int, ProfileInterface)]

  /**
   * Création d'un nouveau profil de joueur.
   * Renvoie la représentation du profil créé et son Id.
   */
  def createProfile(name: String, descr: String): (Int, ProfileInterface)

  /**
   * Renvoie les différentes sauvegardes de personnages disponibles
   * pour le profil donné.
   */
  def getCharacters(profileId: Int): List[(Int, SavedCharacterInterface)]

  /**
   * Renvoie la liste des classes disponibles dans le jeu.
   */
  def getClasses(): List[(Int, ClassInterface)]

  /**
   * Création d'un nouveau personnage.
   * Renvoie la représentation du personnage créé et son Id.
   */
  def createCharacter(playerId: Int, name: String, descr: String, charClassId: Int): (Int, SavedCharacterInterface)

  /**
   * Création du contrôleur in game par défaut.
   */
  def createGame(profileId: Int, characterId: Int): GameControl

  /**
   * Création du contrôleur in game avec des extensions.
   */
  def createGame(profileId: Int, characterId: Int, extensionsIds: List[Int]): GameControl
}

/** Une usine pour créer le InitControl. */
object InitControl {
  def apply(out: Out): InitControl = new ConcreteInitControl(out)
  def apply(out: Out, extensionsIds: List[Int]): InitControl = {
    this(out)
  }
}

abstract class InitControl extends Init {}

class ConcreteInitControl(out: Out) extends InitControl {
  val profiles = new HashMap[Int, Player]
  var nbPlayers = 0
  loadProfiles()

  private def loadProfiles() = {
    //Quelques fausses données, pour tester
    val wag = Player("Wagyl", "Patpat.")
    wag.addChar(Character(Identifier.sorcerer, "Wagyl", ""))
    val k20 = Player("Kbarat", "Vous connaissez pas les bugnes ?!")
    k20.addChar(Character(Identifier.warrior, "K20", ""))
    val odp = Player("Odp", "Aïe ! Scrogneugneu !")
    odp.addChar(Character(Identifier.warrior, "Odp", ""))
    val rain = Player("Rain", "Et paf ! Ça fait des Chocapics !!")
    rain.addChar(Character(Identifier.rogue, "Rain", ""))
    profiles.put(0, wag)
    profiles.put(1, k20)
    profiles.put(2, odp)
    profiles.put(3, rain)
    nbPlayers = 4
  }

  override def getPreferences(): PrefsInterface = {
    null
  }

  override def setPreferences(pref: PrefsInterface): Unit = {
  }

  override def getProfiles(): List[(Int, ProfileInterface)] = {
    val rc = profiles.map(p ⇒ (p._1, p._2.initRepresentation()))
    rc.toList
  }

  override def createProfile(name: String, descr: String): (Int, ProfileInterface) = {
    val p = Player(name, descr)
    profiles.put(nbPlayers, p)
    val rc = (nbPlayers, p.initRepresentation)
    nbPlayers += 1
    rc
  }

  override def getCharacters(profileId: Int): List[(Int, SavedCharacterInterface)] = {
    val profile = profiles.get(profileId)
    profile match {
      case None ⇒ Nil
      case Some(p) ⇒ {
        val rc = p.characters.map(c ⇒ (c._1, c._2.initRepresentation()))
        rc.toList
      }
    }
  }

  override def getClasses(): List[(Int, ClassInterface)] = {
    var classes: List[(Int, ClassInterface)] = Nil
    classes
  }

  //TODO choisir l'ID plus joliment.
  override def createCharacter(profileId: Int, name: String, descr: String, charClassId: Int): (Int, SavedCharacterInterface) = {
    val c = Character(charClassId, name, descr)
    val profile = profiles.get(profileId)
    profile match {
      case None ⇒ throw new ControllerException("Joueur inexistant")
      case Some(p) ⇒ {
        p.addChar(c)
        (p.nbChars - 1, c.initRepresentation)
      }
    }
  }

  override def createGame(profileId: Int, characterId: Int): GameControl = {
    this.createGame(profileId, characterId, Nil)
  }

  override def createGame(profileId: Int, characterId: Int, extensionsIds: List[Int]): GameControl = {
    val player = profiles.get(profileId)
    player match {
      case None ⇒ throw new ControllerException("Joueur inexistant")
      case Some(p) ⇒ {
        val char = p.characters.get(characterId)
        char match {
          case None ⇒ throw new ControllerException("Personnage inexistant")
          case Some(c) ⇒ GameControl(c, out, extensionsIds)
        }
      }
    }
  }
}
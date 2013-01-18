package xml

import scala.collection.mutable.HashMap
import scala.util.Random

/**
 * Des expérimentations avec le XML dans l'espoir de s'en servir pour internationaliser le projet.
 *
 * INUTILISÉ.
 *
 */

object Test extends App {
  val t = new Translation("fr_FR.xml")
  val gob = t.getTranslation(1001)
  println(gob.name + " : " + gob.randomDescription())
  val u = t.getTranslation(7) //objet inexistant
  println(u.name + " : " + u.randomDescription())
}

/**
 * Représente une langue, par exemple :
 * language = fr, location = France, name = Français
 */
class Localization(val language: String, val location: String, val name: String) {}

/**
 * Traductions d'une entité de base (nom et description(s)) dans une langue donnée
 */
class EntityTranslation(
  val id: Int,
  val language: Localization,
  val name: String,
  val descriptions: List[String]) {
  val random = new Random()

  def randomDescription(): String = {
    if (descriptions.length == 0) {
      return ""
    }
    val index = random.nextInt(descriptions.length)
    descriptions(index) //TODO OutOfBounds? 
  }
}

/**
 * Une traduction spéciale, pour représenter
 * une entité n'ayant aucune traduction
 * (ce qui ne devrait pas arriver)
 */
class EmptyEntityTranslation(id: Int, loc: Localization)
  extends EntityTranslation(id, loc, "", Nil) {}

/**
 * Gestion des traductions des entités dans une langue donnée.
 * Si la traduction dans la langue demandée n'est pas trouvée,
 * la traduction dans la langue par défaut (français) sera utilisée.
 */
class Translation(val filename: String) {
  val defaultLanguage = "fr_FR.xml"
  val locz: Localization = initLocz()
  val translatedEntities = new HashMap[Int, EntityTranslation]()
  reading(defaultLanguage)
  reading(filename)

  def getTranslation(id: Int): EntityTranslation = {
    val rc = translatedEntities.get(id)
    rc match {
      case None ⇒ new EmptyEntityTranslation(id, locz)
      case Some(e) ⇒ e
    }
  }

  private def initLocz(): Localization = {
    val infos = scala.xml.XML.loadFile(filename)
    val lang = (infos \ "@language").text
    val loc = (infos \ "@location").text
    val name = (infos \ "@name").text
    new Localization(lang, loc, name)
  }

  private def reading(filename: String) {
    val infos = scala.xml.XML.loadFile(filename)

    (infos \ "monster").foreach { monster ⇒

      val id = (monster \ "@id").text.toInt
      val name = (monster \ "name").text
      var descrs: List[String] = Nil

      (monster \ "descr").foreach { descr ⇒
        descrs = (descr.text) :: descrs
      }

      val translatedEntity = new EntityTranslation(id, locz, name, descrs)
      translatedEntities.put(id, translatedEntity)
    }
  }
}
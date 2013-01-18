package representation

/**
 * Trait implémenté par les objets d'initialisation qui doivent être envoyés à l'interface graphique.
 */
trait ConcreteInits {
  def initRepresentation(): InitsInterface
}

/**
 * Représentation des données d'initialisation d'une partie pour l'interface utilisateur.
 */
abstract class InitsInterface(val name: String) {
  var description: String
}

/**
 * Représentation des préférences du jeu.
 */
abstract class PrefsInterface(name: String)
  extends InitsInterface(name) {
  var preferences: List[(Int, PreferenceInterface)] = List()
}

/**
 * Représentation d'une préférence du jeu.
 */
abstract class PreferenceInterface(name: String)
  extends InitsInterface(name) {
  val value: String
}

/**
 * Représentation du profil d'un joueur.
 */
abstract class ProfileInterface(name: String, nbChars: Int)
  extends InitsInterface(name) {
}

/**
 * Représentation du personnage d'un joueur.
 */
abstract class SavedCharacterInterface(name: String)
  extends InitsInterface(name) {
  val classInf: ClassInterface
  //val land TODO map/position du joueur
  val gender: Int
  val stats: StatSetInterface
}

/**
 * Représentation d'une classe du jeu.
 */
abstract class ClassInterface(name: String, baseHealth: Float, baseStats: StatSetInterface)
  extends InitsInterface(name) {
  def identifier: Int
}

// TODO

/**
 * Représentation des statistiques d'un personnage.
 */
abstract class StatSetInterface(name: String)
  extends InitsInterface(name) {
  var stats: List[(Int, StatInterface)] = List()
}

/**
 * Représentation d'une statistique d'un personnage.
 */
abstract class StatInterface(name: String)
  extends InitsInterface(name) {
  val value: Int
}
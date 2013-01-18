package controller

import changes._
import objects._
import data._
import representation._

/*** Façades in game entre les contrôleurs et les interfaces graphiques. ***/

/**
 * Façade implémentée par les contrôleurs.
 */
trait In {
  /**
   * Réalisation d'une action appartenant au joueur.
   */
  def make(actionId: Int): Result

  /**
   * Réalisation d'une action Use sur une entité Usable.
   */
  def use(entityId: Int, useId: Int): Result

  /**
   * Réalisation d'une action Consume sur une entité Edible.
   */
  def consume(entityId: Int, edibleId: Int): Result

  /**
   * Sauvegarder la partie.
   */
  def save(): Unit

  /**
   * Quitter la partie.
   */
  def quit(save: Boolean): Unit
}

/**
 * Façade implémentée par les interfaces graphiques.
 */
trait Out {
  /**
   * Renvoie le couple (identifiant temporaire, (représentation du) personnage).
   * Si le personnage ou la représentation du personnage est introuvable (ce qui ne devrait jamais arriver),
   * l'exception ControllerException est lancée.
   */
  def character(data: (Int, SelfInterface)): Unit

  /**
   * Liste des (représentations des) objets de la carte que le joueur voit.
   */
  def objects(data: List[(Int, EntityInterface)]): Unit

  /**
   * Liste des (représentations des) objets corespondant aux cases de la carte que le joueur voit.
   */
  def allObjects(data: List[((Int, TileInterface), List[(Int, EntityInterface)])]): Unit

  /**
   * Partie de la carte que le joueur voit.
   */
  def maps(data: LandInterface): Unit

  /**
   * Liste des (représentations des) objets que le joueur a dans son inventaire.
   */
  def inventory(data: List[(Int, ContentInterface)]): Unit

  /**
   * Evénements du jeu signalés à l'interface graphique.
   */
  def event(data: EventInterface): Unit
}
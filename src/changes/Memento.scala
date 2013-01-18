package changes

import objects._

/*
 * Un Memento est un couple de fonctions à insérer dans un Change,
 * qui seront exécutées l'une juste avant et l'autre juste après
 * l'exécution du changement concret.
 *
 * Ces fonctions doivent servir à retenir le changement exact effectué,
 * si besoin.
 *
 * Elles ne devraient PAS effectuer elles-mêmes un changement.
 * S'il est nécessaire d'étendre le Change, mieux vaut l'étendre via les formes
 * logiques Sequence ou ConditionalBranch.
 */

abstract class Memento {
  def before (e : Entity, t : Entity) : Unit
  def after (e : Entity, t : Entity) : Unit
}

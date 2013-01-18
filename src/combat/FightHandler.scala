package combat

import objects._

/* Objet qui gère un coup */
object Fight {
  type T = Entity with StatHandler
  var combatSheetType: Option[Unit => CombatSheet] = None

  /* Initialisation de la feuille de combat */
  def initCombatSheet(initVal: Unit => CombatSheet): Unit =
    combatSheetType = Some(initVal)

  /* Méthode pour avoir une feuille de combat. 
   * Si aucun type n'a été "initialisé" on utilise la feuille basique */
  def getCombatSheet(): CombatSheet =
    combatSheetType match {
      case Some(f) => f()
      case None => new BasicCombatSheet
    }

  /* Méthode de déclanchement du calcul d'un coup */
  def handleFight(effector: T, target: T, spell: SpellDef): CombatResult =
    spell.cast(effector, target, getCombatSheet)

}

/* La feuille de combat abstraite */
abstract class CombatSheet {
  var baseValue: Int
  var castTime: Double

  def compute: CombatResult
}

/* Résultat d'un combat */
abstract class CombatResult(v: Int, t: Double) {
  val res: Int = v
  val time: Double = t

  override def toString = res + " - " + time
}

case class CompositeCombatResult(children: List[CombatResult])
  extends CombatResult({
    var tmp: Int = 0
    children foreach { tmp += _.res }
    tmp
  }, {
    var tmp: Double = 0
    children foreach { tmp += _.time }
    tmp
  }) {
  override def toString = {
    val res: Seq[String] = for (a <- children) yield a.toString
    res.toString
  }
}

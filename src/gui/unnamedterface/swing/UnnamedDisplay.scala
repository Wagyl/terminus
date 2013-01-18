/*
 * DEPRECATED
 * Fichier non mis à jour malheureusement.
 */

package gui.unnamedterface.swing

import data.Land
import representation.SelfInterface
import representation.ObjectInterface
import scala.swing.TextArea
import scala.swing.GridPanel
import gui.unnamedterface.UnnamedTile
import scala.swing.ListView

/* Affichage du jeu */
class UnnamedDisplay extends GridPanel(10, 10) with Refreshable {

  var offsetX: Int = 0
  var offsetY: Int = 0

  def buildData(map: Land, player: SelfInterface,
    objects: List[ObjectInterface]) = ()

  def drawObject(x: Int, y: Int, toDraw: UnnamedTile) = ()

  override def refresh = ()
}

/* Classe d'affichage du log */
class UnnamedLog extends TextArea {
  def log(toLog: String): Unit = append(toLog + "\n")
}

/* Classe d'affichage des actions */
class UnnamedActions extends ListView with Refreshable {
  override def refresh = ()
}

/* Trait */
trait Refreshable {
  def refresh: Unit
}
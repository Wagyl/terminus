/*
 * DEPRECATED
 * Fichier non mis à jour malheureusement.
 */

package gui.unnamedterface.swing

import scala.swing.SimpleSwingApplication
import scala.swing.MainFrame
import scala.swing.BorderPanel
import scala.swing.Component
import java.awt.Dimension

abstract class UnnamedFrame extends SimpleSwingApplication with Refreshable {

  val log: Component
  val actions: Component with Refreshable
  val display: Component with Refreshable
  val frameTitle: String

  def top = new MainFrame {
    title = frameTitle
    contents = new BorderPanel {
      add(log, BorderPanel.Position.South)
      add(actions, BorderPanel.Position.West)
      add(display, BorderPanel.Position.Center)
    }
    size = new Dimension(900, 650)
  }

  override def refresh = {
    display.refresh
    actions.refresh
  }
}
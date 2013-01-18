package gui.simplynterface

import data._
import representation._
import scala.swing.BorderPanel
import scala.swing.GridPanel
import scala.swing.Panel
import scala.swing.Label
import javax.swing.ImageIcon
import java.awt.Dimension

class GraphicDisplay(frame: FrameDisplay) extends SwingDisplay(frame) {

  override def printMap(map: LandInterface, player: SelfInterface,
    objects: List[ObjectInterface]): Unit = player.position match {

    case Coordinates(x, y, z) =>

      val width = 11
      val height = 11

      val images = new Array[Label](width * height)

      for (y <- 0 until height) {
        for (x <- 0 until width) {
          val empty = new Label {
            icon = new ImageIcon("images/unseen.png")
          }
          images.update(x + (height - y - 1) * width, empty)
        }
      }

      def index(c: Coordinates): Int = {
        val i = c.x - x + width / 2
        val j = c.y - y + height / 2
        if (i < 0 || i >= width || j < 0 || j >= height || c.z != z)
          -1
        else
          i + (height - j - 1) * width
      }

      var tile = map.tiles.map(o => (index(o._2.position), getImg(o._2)))
      tile = tile.filter(_._1 > 0)

      tile.foreach(o => images.update(o._1, o._2))

      var obj = objects.map(o => (index(o.position), getImg(o)))
      obj = obj.filter(_._1 > 0)

      obj.foreach(o => images.update(o._1, o._2))

      images.update((width / 2) + (height / 2) * width, getImg(player))

      val gridpanel = new GridPanel(width, height) {
        images.foreach(contents += _)
      }

      this.frame.mappanel.contents.clear
      this.frame.mappanel.contents += gridpanel
      this.frame.mappanel.revalidate()

    case _ => return
  }

  def getImg(e: { def identifier: Int }): Label = {
    val name = e.identifier match {
      case Identifier.character => "human.png"
      case Identifier.goblin => "goblin.png"
      case Identifier.doorOpen => "open_door.png"
      case Identifier.doorClosed => "closed_door.png"
      case Identifier.empty => "unseen.png"
      case Identifier.wall => "stone2_gray1.png"
      case Identifier.ground => "crypt0.png"
      case Identifier.fountain => "blue_fountain.png"
      case Identifier.flask => "potion_cloudy.png"
      case Identifier.snake => "skeleton_snake.png"
      case Identifier.transparent_wall => "transparent_wall.png"
      case Identifier.altar => "altar.png"
      case Identifier.statue => "statue.png"
      case _ => "unseen.png"
    }

    val path = "images/" + name

    val l = new Label {
      icon = new ImageIcon(path)
    }

    l

  }

}

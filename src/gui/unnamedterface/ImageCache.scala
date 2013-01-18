package gui.unnamedterface

import scala.collection.mutable.HashMap
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object ImageCache {

  var imageMap: HashMap[Int, BufferedImage] = new HashMap[Int, BufferedImage]

  val defaultImagePath: String = "images/unseen.png"

  val defaultImage: BufferedImage = ImageIO.read(new File(defaultImagePath))

  def addImage(toAdd: BufferedImage, id: Int): Unit =
    imageMap += (id -> toAdd)

  def addImage(toAdd: String, id: Int): Unit =
    addImage(ImageIO.read(new File(toAdd)), id)

  def getImage(id: Int): BufferedImage = imageMap.get(id) match {
    case Some(x) => x
    case None => defaultImage
  }
}
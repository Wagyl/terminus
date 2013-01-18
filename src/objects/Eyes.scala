package objects

import representation._
import data._

/*
 * Trait des Entity percevant leur environnement.
 */

trait Eyes extends Entity {

  def eyesight : Int
  def center : Int = eyesight

  implicit def boolToInt (b : Boolean) : Int = if (b) 1 else 0

  class Square {
    var topleft : Angle = new Angle
    var topright : Angle = new Angle
    var bottomright : Angle = new Angle
    var bottomleft : Angle = new Angle

    def isVisible : Boolean = {
      ( topleft.isVisible + topright.isVisible +
	bottomright.isVisible + bottomleft.isVisible > 1)
    }
  }

  class Angle {
    var tl : Boolean = true
    var tr : Boolean = true
    var br : Boolean = true
    var bl : Boolean = true

    def isVisible : Int = tl + tr + br + bl
  }

  def seen () : List[(Tile, List[Entity])] = {
    
    this.position match {
      case c : Coordinates =>
      
      val max = eyesight * 2 + 1

      /*
       * 0 : visible
       * 1 : partiellement caché
       * 2 : caché.
       */
      val plots = new Array[Square](max * max)
      for(i <- 0 until max * max)
	plots(i) = new Square

      val visited = new Array[Boolean](max * max)
      
      var view : List[Option[(Tile, List[Entity])]] = List()

      // Coin supérieur droit
      for (x <- center until max) {
	for (y <- center until max) {
	  val v = checkPoint(Point(x, y), max, plots)
	  if (!visited(x * max + y))
	    view ::= v
	  visited(x * max + y) = true
	}
      }

      // Coin inférieur droit
      for (x <- center until max) {
	for (y <- center to 0 by -1) {
	  val v = checkPoint(Point(x, y), max, plots)
	  if (!visited(x * max + y))
	    view ::= v
	  visited(x * max + y) = true
	}
      }

      // Coin inférieur gauche
      for (x <- center to 0 by -1) {
	for (y <- center to 0 by -1) {
	  val v = checkPoint(Point(x, y), max, plots)
	  if (!visited(x * max + y))
	    view ::= v
	  visited(x * max + y) = true
	}
      }

      // Coin supérieur gauche
      for (x <- center to 0 by -1) {
	for (y <- center until max) {
	  val v = checkPoint(Point(x, y), max, plots)
	  if (!visited(x * max + y))
	    view ::= v
	  visited(x * max + y) = true
	}
      }
      

      view.flatMap(x => x)

    case _ => List()
    }
  }


  private final def checkPoint (current : Point,
    max : Int, plots : Array[Square]) : Option[(Tile, List[Entity])] = {

      val sqr = plots(current.x * max + current.y)
      if (!sqr.isVisible) {
	None
      }
      else
	this.position match {
	  case Coordinates(x, y, z) =>
	  val position = Coordinates(x-center+current.x, y-center+current.y, z)
	  val tile = this.game.world.getTileAt(position)
	  val entities = this.game.data.getEntitiesAt(position)
	  if (!tile.isTransparent || entities.exists(!_.isTransparent))
	    update(current, max, plots)
	  Some(tile, entities)
	  
	  case _ => None
	}
    }


  // Vector est déjà pris. :(
  case class Point (x : Int, y : Int) { }


  def vector (A : Point, B : Point) : Point = {
    Point (B.x - A.x, B.y - A.y)
  }

  def cross (U : Point, V : Point) : Int = {
    U.x * V.y - U.y * V.x
  }

  /*
   * Mise à jour du tableau plots
   */
  def update (obstacle : Point, max : Int, plots : Array[Square]) : Unit = {
    
    if (obstacle.x == center && obstacle.y == center)
      return

    // Coin supérieur droit
    if (obstacle.x >= center && obstacle.y >= center) {
      val o1 = Point(obstacle.x, obstacle.y+1)
      val o2 = Point(obstacle.x+1, obstacle.y)
      for (i <- obstacle.x until max)
	for (j <- obstacle.y until max)
	  if (i != obstacle.x || j != obstacle.y) {
	    val sq = plots(i * max + j)
	    val rv = updateValue(o1, o2, Point(i, j), sq)
	    plots(i * max + j) = rv
	  }
    }

    // Coin inférieur droit
    if (obstacle.x >= center && obstacle.y <= center) {
      val o1 = Point(obstacle.x+1, obstacle.y+1)
      val o2 = Point(obstacle.x, obstacle.y)
      for (i <- obstacle.x until max)
	for (j <- obstacle.y to 0 by -1)
	  if (i != obstacle.x || j != obstacle.y) {
	    val sq = plots(i * max + j)
	    val rv = updateValue(o1, o2, Point(i, j), sq)
	    plots(i * max + j) = rv
	  }
    }

    // Coin inférieur gauche
    if (obstacle.x <= center && obstacle.y <= center) {
      val o1 = Point(obstacle.x+1, obstacle.y)
      val o2 = Point(obstacle.x, obstacle.y+1)
      for (i <- obstacle.x to 0 by -1)
	for (j <- obstacle.y to 0 by -1)
	  if (i != obstacle.x || j != obstacle.y) {
	    val sq = plots(i * max + j)
	    val rv = updateValue(o1, o2, Point(i, j), sq)
	    plots(i * max + j) = rv
	  }
    }
    
    // Coin supérieur gauche
    if (obstacle.x <= center && obstacle.y >= center) {
      val o1 = Point(obstacle.x, obstacle.y)
      val o2 = Point(obstacle.x+1, obstacle.y+1)
      for (i <- obstacle.x to 0 by -1)
	for (j <- obstacle.y until max)
	  if (i != obstacle.x || j != obstacle.y) {
	    val sq = plots(i * max + j)
	    val rv = updateValue(o1, o2, Point(i, j), sq)
	    plots(i * max + j) = rv
	  }
    }
  }

  def updateValue (obstacle1 : Point, obstacle2 : Point, 
    p : Point, square : Square) : Square = {

    // Origine du rayon...
    val otl = Point (center, center+1)
    val otr = Point (center+1, center+1)
    val obr = Point (center+1, center)
    val obl = Point (center, center)

    // Point d'impact du rayon...
    val ptl = Point (p.x, p.y+1)
    val ptr = Point (p.x+1, p.y+1)
    val pbr = Point (p.x+1, p.y)
    

    def value (point : Point, origin : Point) : Boolean = {
      val vect = vector(origin, point)
      if (origin == obstacle1 || origin == obstacle2)
	return true
      val v1 : Boolean =
	if (origin.x == obstacle1.x || origin.y == obstacle1.y)
	  cross(vector(origin, obstacle1), vect) > 0
	else
	  cross(vector(origin, obstacle1), vect) >= 0
      val v2 : Boolean =
	if (origin.x == obstacle2.x || origin.y == obstacle2.y)
	  cross(vector(origin, obstacle2), vect) < 0
	else
	  cross(vector(origin, obstacle2), vect) <= 0
      v1 || v2
    }

    square.topleft.tl &&= value(ptl, otl)
    square.topleft.tr &&= value(ptl, otr)
    square.topleft.br &&= value(ptl, obr)
    square.topleft.bl &&= value(ptl, obl)

    square.topright.tl &&= value(ptr, otl)
    square.topright.tr &&= value(ptr, otr)
    square.topright.br &&= value(ptr, obr)
    square.topright.bl &&= value(ptr, obl)

    square.bottomright.tl &&= value(pbr, otl)
    square.bottomright.tr &&= value(pbr, otr)
    square.bottomright.br &&= value(pbr, obr)
    square.bottomright.bl &&= value(pbr, obl)

    square.bottomleft.tl &&= value(p, otl)
    square.bottomleft.tr &&= value(p, otr)
    square.bottomleft.br &&= value(p, obr)
    square.bottomleft.bl &&= value(p, obl)

    square
  }
}

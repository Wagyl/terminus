package tests

import objects._
import changes._
import data._
import representation._

/*
 * Quelques tests.
 *
 */

object EyesTest {


  // Vector est déjà pris. :(
  case class Point (x : Int, y : Int) { }

  def horizontalStraight (a : Point, b : Point) : Boolean = {
    
    val dx = b.x - a.x
    val dy = b.y - a.y

    var ed = 2 * math.abs(dy) - dx
    val incr1 = 2 * math.abs(dy) - 2 * dx
    val incr2 = 2 * math.abs(dy)

    var y = a.y
    for(x <- a.x until b.x) {
      if (ed > dx) {
	ed += incr1
	if (dy > 0) y += 1; else y -= 1
	println("(" + (x-1) + ", " + y +")")
      }
      else
	ed += incr2
      println("(" + (x) + ", " + y +")")
    }
    
    true;
  }


  def main(args:Array[String]) {
    

    horizontalStraight(Point(2, 5), Point(6, 9))


  }

}

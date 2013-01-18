package data

/*
 * Classe abstraite des générateurs de données
 */

abstract class Generator (val game : Game) {

  def fillData (position : Coordinates) : Unit

}



object Generator {

  def apply (typ : LandType, game : Game) : Generator = typ match {
    case Default (_) => new LandGenerator(game)
  }

}


sealed abstract class Size { }
case class Finite (width : Int, height : Int) extends Size { }
case class Infinite () extends Size { }


sealed abstract class LandType { }
case class Default (size : Size) extends LandType { }

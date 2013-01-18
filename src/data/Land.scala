package data

import representation.Identifier

/**
 *  Représentation d'une carte 2D
 *
 */
abstract class Land {
  def width: Int
  def height: Int
  def floor: Floor

  def name : String
  def description : String
  
  def generator : Generator

  def getTile(x: Int, y: Int): Tile
  def setTile(x: Int, y: Int, t: Tile): Unit
}

object Land {

  def apply (typ : LandType, f : Floor, n : String, desc : String,
    game : Game) : Land = typ match {

    case Default (size) => 
    val g = Generator(typ, game)
    Land(size, f, g, n, desc)
  }


  def apply (size : Size, f : Floor, g : Generator,
    n : String, desc : String) : Land = 
    size match {
      case Finite (w, h) => new FiniteMap (w, h, f, g, n, desc)
      case Infinite () => new InfiniteMap (-1, -1, f, g, n, desc)
    }
}



/**
 *  Représentation logique de la carte
 *
 */
abstract class RealMap extends Land {
  
  def tile (x : Int, y : Int) : Char

  override final def getTile (x : Int, y : Int) : Tile = {
    /*
     * Si la coordonnée est bien à l'intérieur de la carte
     * (ou si la carte est infinie) ...
     */
    if ( (width < 0 || (x >= 0 && x < width))
      && (height < 0 || (y >= 0 && y < height)) ) {

      /* charactère correspondant dans le tableau */
      val char = tile(x, y)
      
      /* Conversion */
      TileFactory.fromMap(char) match {
	/* S'il y a bien un Tile, on place la bonne position et on le renvoie */
	case Some(t) => positionne(t, x, y)
	case None => 
	/* Sinon, appel du générateur et deuxième essai */
	generator.fillData(Coordinates(x, y, this.floor))
	TileFactory.fromMap(char) match {
	  case Some(t) => positionne(t, x, y)
	  /* Si toujours pas, Empty par défaut. */
	  case None => positionne(Empty(), x, y)
	}
      }
    }
    else
      if (x == -1 || x == width || y == -1 || y == height)
	positionne(Wall(), x, y)
      else
	positionne(Empty(), x, y)
  }

  private def positionne (tile : Tile, x : Int, y : Int) : Tile = {
    tile.position = Coordinates(x, y, this.floor)
    tile
  }

  
}


/*
 * Carte finie.
 * w et h > 0
 */

class FiniteMap (w : Int, h : Int, f : Floor, g : Generator,
  n : String, desc : String) extends RealMap {

  val width = w
  val height = h
  val floor = f
  val generator = g
  val name = n
  val description = desc
  
  /*
   * Tableau des tiles de la carte, sous forme de charactère
   * (table de conversion dans TileFactory)
   */
  private var map = new Array[Char](width * height)

  override def tile (x : Int, y : Int) : Char = this.map((x * width) + y)

  override def setTile(x: Int, y: Int, t: Tile) = {
    if (x >= 0 && x < width && y < height && y >= 0)
      this.map((x * width) + y) = TileFactory.toMap(t)
  }

  def fillWith(t: Tile) {
    val c = TileFactory.toMap(t)
    for (i <- 0 until this.map.length)
      this.map(i) = c
  }
}



/*
 * Carte infinie.
 */

class InfiniteMap (val w : Int, h : Int, f : Floor, g : Generator,
  n : String, desc : String) extends RealMap {
  
  val width = w
  val height = h
  val floor = f
  val generator = g
  val name = n
  val description = desc

  /*
   *
   */
  private var map : Map[(Int, Int), Char] = Map[(Int, Int), Char]()

  override def tile (x : Int, y : Int) : Char = 
    try {
      this.map((x, y))
    } catch {
      case _ : NoSuchElementException => Identifier.indeterminate.toChar
    }
  
  override def setTile(x: Int, y: Int, t: Tile) = {
    this.map += (((x, y), TileFactory.toMap(t)))
  }
}

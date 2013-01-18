package data

import objects._

/*
 * Un générateur naif.
 * ATTENTION : ne fonctionne que sur des lands carrés de tailles multiples de 8 !
 */

class LandGenerator (game : Game) extends Generator (game) {

  // Taille des parcelles.
  val size = 8
  val ground = new Ground()
  val wall = new Wall()
  val transwall = new TransparentWall()

  
  private def round (a : Int, n : Int) : Int = {
    val r = a % n match {
      case x if x >= 0 => x
      case x if x < 0 => n + x
    }
    a - r
  }


  override def fillData (position : Coordinates) : Unit = {

    def data = game.data

    val floor = position.z
    val land = game.world.land(floor)
    val max = land.width

    val x0 = round(position.x, size)
    val y0 = round(position.y, size)

    fillLand(data, land, x0, y0)

    
    val x1 = scala.util.Random.nextInt(size)
    val y1 = scala.util.Random.nextInt(size)
    val c1 = Coordinates(x0 + x1, y0 + y1, floor)

    if (game.isStackable(c1))
      data.addEntityAt(Goblin.get, c1)
    

    val x2 = scala.util.Random.nextInt(size)
    val y2 = scala.util.Random.nextInt(size)
    val c2 = Coordinates(x0 + x2, y0 + y2, floor)

    if (game.isStackable(c2))
      data.addEntityAt(Snake.get, c2)
  }


  def fillLand (data : Data, land : Land, x : Int, y : Int) : Unit = {
    
    val max = 13
    var r = scala.util.Random.nextInt(max)

    r match {
      case 0 | 1 => fillOne(data, land, x, y)
      case 2 | 3 => fillTwo(data, land, x, y)
      case 4 | 5 => fillThree(data, land, x, y)
      case 6 | 7 => fillFour(data, land, x, y)
      case 8 => fill5(data, land, x, y)
      case 9 | 10 => fill6(data, land, x, y)
      case 11 | 12 => fill7(data, land, x, y)
      case _ => fillZero(data, land, x, y)
    }
  }


  def fillZero (data : Data, land : Land,  x : Int, y : Int) : Unit = {

    def setTile (t : Tile, i : Int, j : Int) : Unit = land.setTile(x+i, y+j, t)

    for (i <- 0 until size)
      for (j <- 0 until size)
	setTile(ground, i, j)
  }

  
  def fillOne (data : Data, land : Land,  x : Int, y : Int) : Unit = {
    val z = land.floor

    def setTile (t : Tile, i : Int, j : Int) : Unit = land.setTile(x+i, y+j, t)
    def setEntity (e : Entity, i : Int, j : Int) : Unit = {
      data.addEntityAt(e, Coordinates(x+i, y+j, z))
    }

    for (i <- 0 until size)
      for (j <- 0 until size)
	setTile(ground, i, j)

    setTile(wall, 0, 0)
    setTile(wall, 0, 1)
    setTile(wall, 0, 2)
    setEntity(new Door(), 0, 3)
    setTile(wall, 0, 4)
    setTile(wall, 0, 5)

    
    if (scala.util.Random.nextInt(1) == 0)
      setEntity(new Fountain, 3, 4)

    if (scala.util.Random.nextInt(1) == 0)
      setEntity(new Flask, 4, 4)

    setTile(wall, 1, 5)
    setTile(wall, 2, 5)
    setTile(wall, 3, 5)
    setTile(wall, 4, 5)
    setTile(wall, 5, 5)
    setTile(wall, 6, 5)

    setTile(wall, 6, 4)
    setEntity(new Door(), 6, 3)
    setTile(wall, 6, 2)
    setTile(wall, 6, 1)
    setTile(wall, 6, 0)

    setTile(wall, 5, 0)
    setTile(wall, 4, 0)
    setEntity(new Door(), 3, 0)
    setTile(wall, 2, 0)
    setTile(wall, 1, 0)

    setTile(wall, 0, 7)
    setTile(wall, 1, 7)
    setTile(wall, 2, 7)
    setEntity(new Door(), 3, 7)
    setTile(wall, 4, 7)
    setTile(wall, 5, 7)
    setTile(wall, 7, 7)
  }


  
  def fillTwo (data : Data, land : Land,  x : Int, y : Int) : Unit = {
    val z = land.floor

    def setTile (t : Tile, i : Int, j : Int) : Unit = land.setTile(x+i, y+j, t)
    def setEntity (e : Entity, i : Int, j : Int) : Unit = {
      data.addEntityAt(e, Coordinates(x+i, y+j, z))
    }

    for (i <- 0 until size)
      for (j <- 0 until size)
	setTile(ground, i, j)
    
    setTile(wall, 0, 0)
    setTile(wall, 0, 1)
    setTile(wall, 0, 2)
    setTile(wall, 0, 4)
    setTile(wall, 0, 5)
    setTile(wall, 0, 6)
    setTile(wall, 0, 7)

    setTile(wall, 1, 7)
    setTile(wall, 2, 7)
    setTile(wall, 4, 7)
    setTile(wall, 5, 7)
    setTile(wall, 6, 7)
    setTile(wall, 7, 7)

    setTile(wall, 7, 6)
    setTile(wall, 7, 5)
    setTile(wall, 7, 4)
    setTile(wall, 7, 2)
    setTile(wall, 7, 1)
    setTile(wall, 7, 0)

    setTile(wall, 7, 0)
    setTile(wall, 6, 0)
    setTile(wall, 5, 0)
    setTile(wall, 4, 0)
    setTile(wall, 2, 0)
    setTile(wall, 1, 0)

    setTile(wall, 2, 2)
    setTile(wall, 2, 3)
    setTile(wall, 2, 5)
    setTile(wall, 3, 5)
    setTile(wall, 5, 5)
    setTile(wall, 5, 3)
    setTile(wall, 5, 2)
    setTile(wall, 3, 2)

  }



  def fillThree (data : Data, land : Land, x : Int, y : Int) : Unit = {
    val z = land.floor

    def setTile (t : Tile, i : Int, j : Int) : Unit = land.setTile(x+i, y+j, t)
    def setEntity (e : Entity, i : Int, j : Int) : Unit = {
      data.addEntityAt(e, Coordinates(x+i, y+j, z))
    }

    for (i <- 0 until size)
      for (j <- 0 until size)
	setTile(ground, i, j)


    setTile(wall, 1, 1)
    setTile(wall, 1, 2)
    setTile(wall, 1, 3)
    setEntity(new Door(), 1, 4)
    setTile(wall, 1, 5)
    setTile(wall, 1, 6)

    setTile(wall, 2, 6)
    setTile(wall, 3, 6)
    setTile(wall, 5, 6)
    setTile(wall, 6, 6)

    setTile(wall, 6, 5)
    setEntity(new Door(), 6, 4)
    setTile(wall, 6, 3)
    setTile(wall, 6, 2)
    setTile(wall, 6, 1)

    setTile(wall, 5, 1)
    setTile(wall, 4, 1)
    setTile(wall, 3, 1)
    setTile(wall, 2, 1)

  }


  def fillFour (data : Data, land : Land, x : Int, y : Int) : Unit = {
    val z = land.floor

    def setTile (t : Tile, i : Int, j : Int) : Unit = land.setTile(x+i, y+j, t)
    def setEntity (e : Entity, i : Int, j : Int) : Unit = {
      data.addEntityAt(e, Coordinates(x+i, y+j, z))
    }

    for (i <- 0 until size)
      for (j <- 0 until size)
	setTile(ground, i, j)

    setTile(wall, 2, 0)
    setTile(wall, 2, 1)
    setTile(wall, 1, 1)
    setTile(wall, 1, 2)
    setTile(wall, 0, 2)

    setTile(wall, 0, 4)
    setTile(wall, 1, 4)
    setTile(wall, 1, 5)
    setTile(wall, 2, 5)
    setTile(wall, 2, 6)
    setTile(wall, 2, 7)

    setTile(wall, 4, 7)
    setTile(wall, 4, 6)
    setTile(wall, 4, 5)
    setTile(wall, 5, 5)
    setTile(wall, 5, 4)
    setTile(wall, 6, 4)
    setTile(wall, 7, 4)

    setTile(wall, 7, 2)
    setTile(wall, 6, 2)
    setTile(wall, 5, 2)
    setTile(wall, 5, 1)
    setTile(wall, 4, 1)
    setTile(wall, 4, 0)

    setTile(wall, 4, 4)
  }

  def fill5 (data : Data, land : Land, x : Int, y : Int) : Unit = {
    val z = land.floor
    
    def setTile (t : Tile, i : Int, j : Int) : Unit = land.setTile(x+i, y+j, t)
    def setEntity (e : Entity, i : Int, j : Int) : Unit = {
      data.addEntityAt(e, Coordinates(x+i, y+j, z))
    }

    for (i <- 0 until size)
      for (j <- 0 until size)
	setTile(ground, i, j)

    setTile(transwall, 1, 1)
    setTile(transwall, 1, 2)
    setTile(transwall, 1, 3)
    setTile(transwall, 1, 4)
    setTile(transwall, 1, 5)
    setTile(transwall, 1, 6)

    setTile(transwall, 6, 1)
    setTile(transwall, 6, 2)
    setTile(transwall, 6, 3)
    setTile(transwall, 6, 4)
    setTile(transwall, 6, 5)
    setTile(transwall, 6, 6)

    setTile(transwall, 2, 1)
    setTile(transwall, 3, 1)
    setTile(transwall, 5, 1)

    setTile(transwall, 2, 6)
    setTile(transwall, 4, 6)
    setTile(transwall, 5, 6)

    setEntity(new Altar, 3, 3)
    setEntity(new Statue, 4, 4)
  }


  def fill6 (data : Data, land : Land, x : Int, y : Int) : Unit = {
    val z = land.floor
    
    def setTile (t : Tile, i : Int, j : Int) : Unit = land.setTile(x+i, y+j, t)
    def setEntity (e : Entity, i : Int, j : Int) : Unit = {
      data.addEntityAt(e, Coordinates(x+i, y+j, z))
    }

    for (i <- 0 until size)
      for (j <- 0 until size)
	setTile(ground, i, j)

    setTile(wall, 0, 0)
    setTile(wall, 0, 1)
    setTile(wall, 0, 2)
    setTile(wall, 0, 4)
    setTile(wall, 0, 5)
    setTile(wall, 0, 6)
    setTile(wall, 0, 7)

    setTile(wall, 7, 0)
    setTile(wall, 7, 1)
    setTile(wall, 7, 2)
    setTile(wall, 7, 4)
    setTile(wall, 7, 5)
    setTile(wall, 7, 6)
    setTile(wall, 7, 7)


    setTile(wall, 1, 0)
    setTile(wall, 2, 0)
    setTile(wall, 4, 0)
    setTile(wall, 5, 0)
    setTile(wall, 6, 0)

    setTile(wall, 1, 7)
    setTile(wall, 2, 7)
    setTile(wall, 4, 7)
    setTile(wall, 5, 7)
    setTile(wall, 6, 7)

  }


  def fill7 (data : Data, land : Land, x : Int, y : Int) : Unit = {
    val z = land.floor
    
    def setTile (t : Tile, i : Int, j : Int) : Unit = land.setTile(x+i, y+j, t)
    def setEntity (e : Entity, i : Int, j : Int) : Unit = {
      data.addEntityAt(e, Coordinates(x+i, y+j, z))
    }

    for (i <- 0 until size)
      for (j <- 0 until size)
	setTile(ground, i, j)

    setTile(wall, 0, 0)
    setTile(wall, 1, 0)
    setTile(wall, 2, 0)
    setTile(wall, 4, 0)
    setTile(wall, 5, 0)
    setTile(wall, 6, 0)
    setTile(wall, 7, 0)

    setTile(wall, 0, 2)
    setTile(wall, 1, 2)
    setTile(wall, 2, 2)

    setEntity(new Fountain(), 3, 4)
    setEntity(new Flask(), 4, 4)

    setTile(wall, 5, 2)
    setTile(wall, 6, 2)
    setTile(wall, 7, 2)
    
    setTile(wall, 0, 4)
    setTile(wall, 0, 5)
    setTile(wall, 0, 6)
    setTile(wall, 0, 7)

    setTile(wall, 7, 4)
    setTile(wall, 7, 5)
    setTile(wall, 7, 6)
    setTile(wall, 7, 7)

    setTile(wall, 1, 7)
    setTile(wall, 2, 7)
    setTile(wall, 4, 7)
    setTile(wall, 5, 7)
    setTile(wall, 6, 7)

    setTile(wall, 2, 3)
    setTile(wall, 2, 4)
    setTile(wall, 2, 5)
    setTile(wall, 3, 5)
    setTile(wall, 4, 5)
    setTile(wall, 5, 5)
    setTile(wall, 5, 4)
    setTile(wall, 5, 3)
  }
}
    

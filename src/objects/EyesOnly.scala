package objects

import representation._
import data._

/*
 * Trait des Entity percevant leur environnement.
 * 
 * L'algorithme construit des lignes de vision entre l'Entity et chaque case
 * de son environnement, et vérifie qu'elles ne rencontrent aucun obstacle.
 * Une ligne de vision valide est une droite (AB) telle que :
 * - A est un des coins de l'Entity
 * - B est un des coins de la case considérée
 * - (AB) traverse bien l'intérieur de la case considérée
 * - toutes les cases traversées par le segment [AB] sont transparentes.
 * Évidemment, toutes les droites allant d'un coin de l'Entity à un coin de
 * la case n'ont pas besoin d'être testées...
 */

trait EyesOnly extends Entity {

  def eyesight : Int
  def center : Int = eyesight

  /*
   * Renvoie un tableau de boolean, à une dimension mais manipulé comme
   * un tableau à deux dimensions, représentant les cases autour de l'Entity.
   */
  def getObstacles (max : Int) : Array[Boolean] =
    this.position match {
      case Coordinates(x, y, z) =>
      val array = new Array[Boolean](max * max)

      /*
       * Construction de obstacles.
       */
      for(i <- 0 until max)
	for(j <- 0 until max)
	  if (i != center || j != center) {
	    val position = Coordinates(x-center+i, y-center+j, z)
	    val tile = this.game.world.getTileAt(position)
	    val entities = this.game.data.getEntitiesAt(position)
	    if (!tile.isTransparent || entities.exists(!_.isTransparent))
	      array(i * max + j) = true
	  }

      array(center*max + center) = false
      
      array

      case _ => new Array[Boolean](0)
  }

  /*
   * Détermine si l'Entity est en mesure d'en voir une autre.
   */
  def see (other : Entity) : Boolean =
    (this.position, other.position) match {
      case (Coordinates(x, y, z), Coordinates(a, b, c)) =>
      if (z != c)
	false
      else {
	val dist = this.distance(other)
	if (dist > math.sqrt(eyesight * eyesight))
	  false
	else {
	  val max = eyesight * 2 + 1
	  val obstacles = getObstacles(max)
	  squareIsSeen(Point(center+a-x, center+b-y), max, obstacles)
	}
      }

      case _ => false
    }
  

  /*
   * Renvoie les tiles et objets de la carte que l'Entity percoit.
   */
  def seen () : List[(Tile, List[Entity])] = {

    this.position match {
      case Coordinates(x, y, z) =>

      val max = eyesight * 2 + 1

      /*
       * Transparence des cases. Tableau centré sur la position de l'Entity.
       */
      val obstacles = getObstacles(max)

      /*
       * Visibilité des cases. Idem.
       */
      val visibles = new Array[Boolean](max * max)

      /*
       * Les objets à une distance plus petite que 1 de l'Entity
       * sont toujours visibles, sans vérification.
       */
      for(i <- -1 to 1)
	for (j <- -1 to 1) {
	  val index = (center+i) * max + (center +j)
	  visibles(index) = true
	}

      
      
      // Coin supérieur droit
      for (x <- center - 1 until max) {
	for (y <- center + 2 until max) {
	  val seen = squareIsSeen(Point(x, y), max, obstacles)
	  visibles(x * max + y) = seen
	  if (!seen)
	    obstacles(x * max + y) = true
	}
      }

      // Coin inférieur droit
      for (x <- center + 2 until max) {
	for (y <- center + 1 to 0 by -1) {
	  val seen = squareIsSeen(Point(x, y), max, obstacles)
	  visibles(x * max + y) = seen
	  if (!seen)
	    obstacles(x * max + y) = true
	}
      }

      // Coin inférieur gauche
      for (x <- center + 1 to 0 by -1) {
	for (y <- center - 2 to 0 by -1) {
	  val seen = squareIsSeen(Point(x, y), max, obstacles)
	  visibles(x * max + y) = seen
	  if (!seen)
	    obstacles(x * max + y) = true
	}
      }

      // Coin supérieur gauche
      for (x <- center - 2 to 0 by -1) {
	for (y <- center - 1 until max) {
	  val seen = squareIsSeen(Point(x, y), max, obstacles)
	  visibles(x * max + y) = seen
	  if (!seen)
	    obstacles(x * max + y) = true
	}
      }

      /*
       * Construction de la liste des entities vues grâce au
       * tableau visibles.
       */

      var view : List[(Tile, List[Entity])] = List()

      for(i <- 0 until max)
	for(j <- 0 until max)
	  if (visibles(i * max + j)) {
	    val position = Coordinates(x-center+i, y-center+j, z)
	    val tile = this.game.world.getTileAt(position)
	    val entities = this.game.data.getEntitiesAt(position)
	    view ::= (tile, entities)
	  }

      view

      /*
       * Si la position de l'Entity n'est pas une coordonnée,
       * il ne voit rien.
       */
      case _ => List()
    }
  }

  /*
   * Détermine si une case du terrain est visible par l'Entity.
   */
  def squareIsSeen (e : Point, 
    max : Int, obstacles : Array[Boolean]) : Boolean = {
    
    val v = obstacles(e.x * max + e.y)
    // la case ne doit pas faire elle-même obstacle...
    obstacles(e.x * max + e.y) = false

    val rv = squareIsSeenAux(e, max, obstacles)
    
    obstacles(e.x * max + e.y) = v
    rv
  }

  def squareIsSeenAux (e : Point, 
    max : Int, obstacles : Array[Boolean]) : Boolean = {
    
    if (e.x > center && e.y > center) {
      if (pointIsSeen (Point(e.x, e.y), max, obstacles))
	return true
      if (pointIsSeen (Point(e.x+1, e.y+1), max, obstacles))
	return true
    }

    if (e.x > center && e.y < center) {
      if (pointIsSeen (Point(e.x, e.y+1), max, obstacles))
	return true
      if (pointIsSeen (Point(e.x+1, e.y), max, obstacles))
	return true
    }


    if (e.x < center && e.y > center) {
      if (pointIsSeen (Point(e.x+1, e.y), max, obstacles))
	return true
      if (pointIsSeen (Point(e.x, e.y+1), max, obstacles))
	return true
    }


    if (e.x < center && e.y < center) {
      if (pointIsSeen (Point(e.x+1, e.y+1), max, obstacles))
	return true
      if (pointIsSeen (Point(e.x, e.y), max, obstacles))
	return true
    }

    if (e.x == center && e.y > center) {
      if (pointIsSeen (Point(e.x+1, e.y+1), max, obstacles))
	return true
      if (pointIsSeen (Point(e.x+1, e.y), max, obstacles))
	return true
    }
    
    if (e.x == center && e.y < center) {
      if (pointIsSeen (Point(e.x, e.y+1), max, obstacles))
	return true
      if (pointIsSeen (Point(e.x, e.y), max, obstacles))
	return true
    }

    if (e.x > center && e.y == center) {
      if (pointIsSeen (Point(e.x, e.y+1), max, obstacles))
	return true
      if (pointIsSeen (Point(e.x+1, e.y+1), max, obstacles))
	return true
    }

    if (e.x < center && e.y == center) {
      if (pointIsSeen (Point(e.x, e.y), max, obstacles))
	return true
      if (pointIsSeen (Point(e.x+1, e.y), max, obstacles))
	return true
    }
    
    false
  }

  /*
   * Détermine si un point est visible par l'Entity.
   */
  def pointIsSeen (p : Point, 
    max : Int, obstacles : Array[Boolean]) : Boolean = {
    
    if (p.x > center) {
      if (straight (p, Point(center+1, center+1), max, obstacles))
	return true
      if (straight (p, Point(center+1, center), max, obstacles))
	return true
    }

    if (p.x < center) {
      if (straight (p, Point(center, center+1), max, obstacles))
	return true
      if (straight (p, Point(center, center), max, obstacles))
	return true
    }

    if (p.y > center) {
      if (straight (p, Point(center, center+1), max, obstacles))
	return true
      if (straight (p, Point(center+1, center+1), max, obstacles))
	return true
    }


    if (p.y < center) {
      if (straight (p, Point(center, center), max, obstacles))
	return true
      if (straight (p, Point(center+1, center), max, obstacles))
	return true
    }

    false
  }


  /*
   * Brehensan
   */

  /*
   * Renvoie true si la droite n'est pas interrompue par un obstacle.
   */
  def straight (a : Point, b : Point,
    max : Int, obstacles : Array[Boolean]) : Boolean = {

    val dx = b.x - a.x
    val dy = b.y - a.y

    (dx, dy) match {
      case (0, 0) => true
      case (dx, dy) if math.abs(dx) >= math.abs(dy) && dx > 0 =>
      horizontalStraight (a, b, max, obstacles)
      case (dx, dy) if math.abs(dx) >= math.abs(dy) && dx < 0 =>
      horizontalStraight (b, a, max, obstacles)
      case (dx, dy) if math.abs(dx) < math.abs(dy) && dy > 0 =>
      verticalStraight (a, b, max, obstacles)
      case (dx, dy) if math.abs(dx) < math.abs(dy) && dy < 0 =>
      verticalStraight (b, a, max, obstacles)
      case _ =>  false
    }
  }

  def horizontalStraight (a : Point, b : Point,
    max : Int, obstacles : Array[Boolean]) : Boolean = {
    
    val dx = b.x - a.x
    val dy = b.y - a.y

    var ed = 2 * math.abs(dy) - dx
    val incr1 = 2 * math.abs(dy) - 2 * dx
    val incr2 = 2 * math.abs(dy)

    var y = if (dy > 0) a.y else a.y-1
    for(x <- a.x until b.x) {
      if (obstacles(x * max + y))
	return false
      if (ed >= dx) {
	if (dy > 0) y += 1; else y -= 1
	if (ed > dx && obstacles(x * max + y))
	  return false
	ed += incr1
      }
      else
	ed += incr2
    }
    
    true;
  }

  def verticalStraight (a : Point, b : Point,
    max : Int, obstacles : Array[Boolean]) : Boolean = {
    
    val dx = b.x - a.x
    val dy = b.y - a.y

    var ed = 2 * math.abs(dx) - dy
    val incr1 = 2 * math.abs(dx) - 2 * dy
    val incr2 = 2 * math.abs(dx)

    var x = if (dx > 0) a.x else a.x - 1
    for(y <- a.y until b.y) {
      if (obstacles(x * max + y))
	return false
      if (ed >= dy) {
	if (dx > 0) x += 1; else x -= 1
	if (ed > dy && obstacles(x * max + y))
	  return false
	ed += incr1
      }
      else
	ed += incr2
    }
    
    true;
  }



  // Vector est déjà pris. :(
  case class Point (x : Int, y : Int) { }

  // Un vecteur à partir de deux points.
  def vector (A : Point, B : Point) : Point = {
    Point (B.x - A.x, B.y - A.y)
  }

  // Produit en croix.
  def cross (U : Point, V : Point) : Int = {
    U.x * V.y - U.y * V.x
  }
}

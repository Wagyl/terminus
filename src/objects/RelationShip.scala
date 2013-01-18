package objects


trait RelationShip extends Entity {

  /*
   * La liste des coteries auxquelles l'entité appartient.
   */
  private var coteries = List[Coterie]()

  /*
   * Les relations que l'entité entretient avec les autres coteries.
   */
  private var relations = List[(Coterie, Relation)]()

  /*
   * La relation par défaut avec les coteries qui ne sont pas dans la liste.
   */
  def relation : Relation = Neutral ()

  final def addCoterie (c : Coterie) : Unit = { coteries ::= c }
  final def addCoteries (c : Coterie*) : Unit = { coteries :::= c.toList}
  final def addRelation (r : (Coterie, Relation)) : Unit = relations ::= r
  final def addRelations (r : (Coterie, Relation)*) : Unit = 
    relations :::= r.toList

  final def relation (other : Entity) : Relation = other match {
    case x : RelationShip => RelationShip(this, x)
    case _ => Neutral ()
  }
  
}




object RelationShip {

  def apply (e1 : RelationShip, e2 : RelationShip) : Relation = {
    val m1 = e1.relations.filter(x => e2.coteries.contains(x._1))
    val m2 = e2.relations.filter(x => e1.coteries.contains(x._1))
    val mutual = e1.relation :: e2.relation :: (m1 ::: m2).map(_._2)
    val enemy = mutual.contains(Enemy ())
    val ally = mutual.contains(Ally ())
    if (enemy && !ally)
      Enemy ()
    else if (!enemy && ally)
      Ally ()
    else
      Neutral ()
  }

}




sealed abstract class Relation { }

case class Ally () extends Relation { }
case class Neutral () extends Relation { }
case class Enemy () extends Relation { }



sealed abstract class Coterie { }
case class Adventurer () extends Coterie { }
case class Monster () extends Coterie { }
case class Animals () extends Coterie { }

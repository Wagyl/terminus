package representation

import data._
import objects.Entity
import communication.parser._

abstract class Representation (val name : String, val identifier : Int) 
{
  /* Courte description à destination du joueur. */
  var description : String
}


object Representation {

  /*
   * Si [elements] est une liste d'objets représentables, i.e. possédant
   * une méthode representation : Entity => Option[T] avec T <: Representation,
   * cette méthode renvoie la liste de couples (index, T) telle que :
   * - les résultats None ont été "effacés"
   * - les index correspondent aux index des objets représentés dans la liste
   * initiale (les index des None ne sont donc pas présents)
   */
  def get[T] (observer : Entity,
    elements : List[ {def representation (observer : Entity) : Option[T]} ])
  : List[(Int, T)] = 
    {
      val optionsRep = elements.map(_.representation(observer))
      val optionsRepIndex = optionsRep.zipWithIndex.map(_.swap)
      val repIndexOptions = optionsRepIndex.map(expand)
      val repIndex = repIndexOptions.flatMap(x => x)
      repIndex
    }

  def expand[A,B] (c : (A,Option[B])) : Option[(A, B)] = c match {
    case (x, Some(y)) => Some((x, y))
    case (_, None) => None
  }

  /*
   *
   */
  def round (value : Double, precision : Int) : Double = { 
    val s = math pow (10, precision)
    (math round value * s) / s
  }
}


/*
 * Trait des représentations pouvant etre traduite dans le protocole
 * de communication utilisé par le module communication.
 */
trait Stream {
  
  /* Atome caractéristique de la classe dans le langage de communication. */
  def lexname () : String

  /* Traduction de l'objet en une liste de lexème. */
  def lexcontent () : List[LexUnit]

  def lexeme () : LexObject = {
    new LexObject(LexAtom(this.lexname), this.lexcontent)
  }
}

/*
 * Méthodes statiques utilitaires sur les Stream.
 */
object Stream {

  def lexeme (list : List[(Int, Stream)]) : LexUnit = {
    val lexlist = list.map(x => LexPair(LexInt(x._1), x._2.lexeme))
    LexList(lexlist)
  }

}

/*
 * Trait des objets compagnions des représentations pouvant etre extraites
 * d'un lexème du protocole de communication.
 */
trait StreamCompanion[E <: Stream] {

  /* Atome caractéristique de la classe dans le langage de communication */
  def lexname () : String

  /*
   * Reconstruction de l'objet d'après une liste de lexème.
   * Cette méthode doit etre l'opération inverse de E.lexcontent.
   */
  def extract (content : List[LexUnit]) : E

  /*
   * Extrait l'objet du LexUnit donné.
   * Throw : ProtocolError
   */
  def extract (lexem : LexUnit) : E = lexem match {
    case lexobj : LexObject =>
    if (lexobj.getName != this.lexname)
      throw ProtocolError ("Ceci ne représente pas un " +this.lexname
	+ " : " +lexem)
    else {
      val content = lexobj.getContent
      extract(content)
    }

    case _ => throw ProtocolError ("Ceci n'est pas un objet : " +lexem)    
  }

  /*
   * Extraction d'un lexème correspondant à une liste de couple (int, E).
   */
  def extractList (lexem : LexUnit) : List[(Int, E)] = {
    val list = lexem.getListValue
    val lexpairs = list.map(lex => (lex.getFirst, lex.getSecond))
    lexpairs.map(x => (x._1.getIntValue, this.extract(x._2)))
  }

}

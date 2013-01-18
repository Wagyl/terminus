package communication.parser

import java.io.OutputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.EOFException;
import java.io.PushbackInputStream
import java.io.ByteArrayOutputStream


/*
 * Erreurs du protocole...
 */

case class ProtocolError (error : String) extends Exception
case class ProtocolParseError (error : String) extends Exception


/*
 * Lexème (unité lexicale) du protocole de communication.
 */

abstract class LexUnit {

  /*
   * Écrit sur l'OutputStream donné en argument le LexUnit selon le formatage
   * du protocole. Doit être surchargée par toutes les implémentations
   * concrètes des unités lexicales du protocole.
   */
  def format (s : OutputStream) : Unit

  /*
   * Cette méthode n'est pas vraiment correcte, elle mélange caractères et
   * octets. Mais elle est bien pratique.
   */
  override def toString () : String = {
    try {
      val s = new ByteArrayOutputStream()
      this.format(s)
      return s.toString("UTF-8")
    } catch {
      case e : IOException => ""
    }
  }

  /*
   * Méthodes d'accès au contenu des lexèmes.
   * Toutes ces méthodes sont surchargées par les sous-classes
   * correspondantes. Mettre des versions ici, plutôt que les rendre
   * abstraites, évite de faire des casts contravariants un peu partout.
   */

  def getStringValue () : String = 
    throw ProtocolError ("Ceci n'est pas une chaîne : " +this)

  def getAtomValue () : String =
    throw ProtocolError ("Ceci n'est pas un atome : " +this)

  def getIntValue () : Int =
    throw ProtocolError ("Ceci n'est pas un entier : " +this)
  
  /* Liste et message */

  def getNthValue (index : Int) : LexUnit =
    throw ProtocolError ("Ceci n'est pas une liste : " +this)

  def getListValue () : List[LexUnit] =
    throw ProtocolError ("Ceci n'est pas une liste : " +this)

  def getLength () : Int =
    throw ProtocolError ("Ceci n'est pas une liste : " +this)

  /* Paires */
  
  def getFirst () : LexUnit =
    throw ProtocolError ("ceci n'est pas une paire : " +this)
  
  def getSecond () : LexUnit =
    throw ProtocolError ("ceci n'est pas une paire : " +this)

  /* Objet */
  
  def getName () : String =
    throw ProtocolError ("Ceci n'est pas un objet : " +this)

  def getContent () : List[LexUnit] =
    throw ProtocolError ("Ceci n'est pas un objet : " +this)
    
}


object LexUnit {

  /*
   * Retourne le premier lexème (LexUnit) correspondant aux caractères lus
   * sur l'inputStream donné en argument.
   * Selon le premier caractère lu, appelle la fonction parse de l'objet
   * correcpondant.
   * throws EOFException, ProtocolParseError.
   */
  def parse (s : PushbackInputStream) : LexUnit = {
    val c : Int = s.read()

    if (c < 0)
      throw new EOFException()
    
    if (c >= 0)
      s.unread(c)

    val lexeme : LexUnit = 
      if (c == '[')
	LexList.parse(s)
      else if ((c >= '0' && c <= '9') || c == '-')
	LexInt.parse(s)
      else if (c == '_' || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
	LexAtom.parse(s)
      else if (c == '<')
	LexPair.parse(s)
      else if (c == '{')
	LexObject.parse(s)
      else if (c == '(')
	LexString.parse(s)
      else {
	throw new ProtocolParseError("Caractère inconnu " + c.asInstanceOf[Char]
	  + " (" + c.asInstanceOf[Int] + ")");
      }
    
    lexeme
  }

}

package communication.parser

import java.io.OutputStream
import java.io.PushbackInputStream

/*
 *
 */

case class LexList (list : List[LexUnit]) extends LexUnit {

  private val value : List[LexUnit] = list


  override def getLength () : Int = value.length

  override def getListValue () : List[LexUnit] = value

  override def getNthValue (index : Int) : LexUnit = value(index)

  /*
   * throws IOException
   */
  override def format (s : OutputStream) : Unit = {
    s.write('[')
    LexList.format(s, value)
    s.write(']')
  }
}


object LexList {

  def format (s : OutputStream, l : List[LexUnit]) : Unit = {
    l.foreach(x => { x.format(s) ; s.write(' ') })
  }

  /*
   * throws ProtocolParseError, IOException
   */
  def parse (s : PushbackInputStream) : LexList = {
    val c1 = s.read()
    if (c1 != '[')
      throw new ProtocolParseError("Ceci n'est pas une liste")
    
    val list = parseList(s)
    
    val c2 = s.read()
    if (c2 != ']')
      throw new ProtocolParseError("Liste mal terminée" +c2);
    
    new LexList(list)
  }


  def parseList (s : PushbackInputStream) : List[LexUnit] = {
    var isList = true
    var c = -1
    var lexlist = List[LexUnit]()

    while (isList) {
      skipEspaces(s)
      c = s.read()
      if (c >= 0)
	s.unread(c)

      if (c < 0 || c == ']' || c == '}' || c == '\n') {
	isList = false
      }
      else {
	val unit : LexUnit = LexUnit.parse(s)
	lexlist ::= unit
      }
    }
    lexlist.reverse
  }

  /*
   * Passe les espaces et les tabulations présents au début de
   * l'inputStream donné en argument.
   */
  def skipEspaces (s : PushbackInputStream) : Unit = {
    var c = -1
    do {
      c = s.read()
    } while (c == ' ' || c == '\r' || c == '\t')
    
    if (c >= 0)
      s.unread(c)
  }

}

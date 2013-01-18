package communication.parser

import java.io.OutputStream
import java.io.PushbackInputStream
import java.io.EOFException;


case class LexString (chain : String) extends LexUnit {

  private val value : String = chain

  override def getStringValue () : String = value
  
  override def format (s : OutputStream) : Unit = {
    s.write('(')
    val bytes = value.getBytes("UTF-8")
    bytes.foreach(x => format(s, x))
    s.write(')')
  }

  private def format (s : OutputStream, c : Byte) = {
    // échappement des caractères spéciauxs
    if ((c >= 0 && c < 0x20) || c == '(' || c == ')' || c == '\\')
      s.write('\\');
    s.write(c);
  }

}

object LexString {
  
  def parse (s : PushbackInputStream) : LexString = {
    var c = s.read()

    if (c != '(')
      throw new ProtocolParseError("Ceci n'est pas une chaîne")
    
    var v = List[Byte]()    
    c = s.read()

    while (c != ')') {
      if (c == '\\')
	c = s.read()
      if (c < 0)
	throw new EOFException()
      v :+= c.toByte
      c = s.read()
    }

    new LexString(new String(v.toArray, "UTF-8"))
  }
    
}

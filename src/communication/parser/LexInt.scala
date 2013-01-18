package communication.parser

import java.io.OutputStream;
import java.io.PushbackInputStream;

/*
 * Représente un entier dans le protocole.
 */

case class LexInt (i : Int) extends LexUnit {

  private val value = i

  override def getIntValue () : Int = value

  /*
   * throws IOException
   */
  override def format (s : OutputStream) : Unit = {
    s.write(("" + value).getBytes("UTF-8"));
  }
}

object LexInt {
  
  /*
   * throws ProtocolParseError, IOException
   */
  def parse (s : PushbackInputStream) : LexInt = {
    var c = -1
    var n = 0
    var neg = false

    c = s.read()
    
    if (c == '-') {
      neg = true
      c = s.read
    }

    if (c < '0' && c > '9')
      throw new ProtocolParseError("Ceci n'est pas un entier")

    do {
      n *= 10
      n += c - '0'
      c = s.read()
    } while (c >= '0' && c <= '9')
    
    if (c >= 0)
      s.unread(c);

    val res = if (neg) -n else n
    return new LexInt(res);
  }
  
}

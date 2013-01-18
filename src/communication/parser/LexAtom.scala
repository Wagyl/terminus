package communication.parser

import java.io.OutputStream
import java.io.PushbackInputStream

/*
 * Représente un atome du protocole, i.e. une suite de caractère ASCII
 * commençant par une lettre et uniquement composée de lettre, de chiffres
 * et de tirets - et _.
 */

case class LexAtom (atom : String) extends LexUnit {

  if (!atom.matches("[A-Za-z_][a-zA-Z0-9_\\-]*"))
    throw new Error("Ceci n'est pas un mot.")
  
  // Contenu du lexatom
  private val value : String = atom

  override def getAtomValue () : String = value

  /*
   * throws IOException
   */
  override def format (s : OutputStream) : Unit = {
    /* Comme un atome ne contient que de l'ASCII, le codage utilisé
     * n'a en principe pas d'importance. */
    s.write(value.getBytes("UTF-8"))
  }
}



object LexAtom {

  /*
   * throws ProtocolParseError, IOException
   */
  def parse (s : PushbackInputStream) : LexAtom = {
    var c = -1
    var atom = true
    val sb : StringBuffer = new StringBuffer()

    c = s.read()

    if (c != '_' && (c < 'A' || c > 'Z') && (c < 'a' || c > 'z'))
      throw ProtocolParseError("Ceci n'est pas un atome")

    while (atom) {
      sb.append(c.asInstanceOf[Char])
      c = s.read()

      if (c != '_' && c != '-' && (c < '0' || c > '9')
	&& (c < 'a' || c > 'z') && (c < 'A' || c > 'Z')) {
	s.unread(c)
	atom = false
      }
    }

    new LexAtom(sb.toString())
  }

}

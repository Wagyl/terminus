package communication.parser

import java.io.OutputStream
import java.io.PushbackInputStream


case class LexPair (fst : LexUnit, snd : LexUnit) extends LexUnit {

  private val first : LexUnit = fst
  private val second : LexUnit = snd

  override def getFirst () : LexUnit = first
  override def getSecond () : LexUnit = second

  override def format (s : OutputStream) : Unit = {
    s.write('<')
    first.format(s)
    s.write(' ')
    second.format(s)
    s.write('>')
  }
}

object LexPair {
  
  def parse (s : PushbackInputStream) : LexPair = {
    val c1 = s.read()
    if (c1 != '<')
      throw new ProtocolParseError("Ceci n'est pas une paire")
    
    LexList.skipEspaces(s)
    val fst : LexUnit = LexUnit.parse(s)
    LexList.skipEspaces(s)
    val snd : LexUnit = LexUnit.parse(s)
    LexList.skipEspaces(s)

    val c2 = s.read()
    if (c2 != '>')
      throw new ProtocolParseError("Paire mal terminée" +c2);

    new LexPair(fst, snd)
  }

}

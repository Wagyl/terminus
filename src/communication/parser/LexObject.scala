package communication.parser

import java.io.OutputStream
import java.io.PushbackInputStream


class LexObject (atom : LexAtom, list : List[LexUnit]) extends LexUnit {

  private val name : LexAtom = atom
  private val value : List[LexUnit] = list

  override def getName () : String = name.getAtomValue
  override def getContent () : List[LexUnit] = value
  
  override def format (s : OutputStream) : Unit = {
    s.write('{')
    name.format(s)
    s.write(' ')
    LexList.format(s, value)
    s.write('}')
  }
}


object LexObject {

  def parse (s : PushbackInputStream) : LexObject = {
    val c1 = s.read()
    if (c1 != '{')
      throw new ProtocolParseError("Ceci n'est pas un objet du protocole.")
    
    val atom = LexAtom.parse(s)
    val list = LexList.parseList(s)
    
    val c2 = s.read()
    if (c2 != '}')
      throw new ProtocolParseError("Objet mal terminé" +c2);
    
    new LexObject(atom, list)
  }

}

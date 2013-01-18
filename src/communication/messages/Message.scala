package communication.messages

import communication._
import communication.parser._
import controller._
import java.io.OutputStream
import java.io.PushbackInputStream
import java.io.EOFException;
import java.io.ByteArrayOutputStream

sealed class Message (val content : List[LexUnit]) {
  def length () : Int = content.length
  def nth (index : Int) : LexUnit = {
    if (index >= 0 && index < length)
      content(index)
    else
      throw ProtocolError("Liste trop courte.")
  }

  /*
   * throws IOException
   */
  def format (s : OutputStream) : Unit = {
    LexList.format(s, content)
    s.write('\n')
  }


  /*
   * Cette méthode n'est pas vraiment correcte, elle mélange
   * caractères et octets. Mais elle est bien pratique.
   */
  override def toString() : String = {
    try {
      val s = new ByteArrayOutputStream()
      format(s)
      return s.toString()
    } catch {
      case _ => "ERROR"
    }
  }
}

abstract class ClientMessage (content : List[LexUnit]) extends Message(content) {
  def process (controller : In, client : Connection) : Unit
}

abstract class ServerMessage (content : List[LexUnit]) extends Message(content) {
  def process (client : Out, pending : Pending, server : Connection) : Unit
}

object Message {

  /*
   * Lit l'inputStream donné en argument et construit le message correspondant.
   * throws EOFException, ProtocolParseError.
   */
  def parse (s : PushbackInputStream) : Message = {
    
    val lexlist = LexList.parseList(s)
    
    val c = s.read()
    if (c < 0)
      throw new EOFException()
    else if (c != '\n')
      throw ProtocolParseError("Caractère inattendu " + c)
    
    if (lexlist.length <= 0)
      throw ProtocolError("Message vide.")
    else {
      lexlist(0) match {
	case lexint : LexInt => sort(lexint.getIntValue, lexlist)
	case x => throw ProtocolError("Message mal formé : ne commence pas par un LexInt : " + (new Message(lexlist)).toString)
      }
    }
  }

  /*
   * Crée le bon type de message selon son code.
   */
  def sort (code : Int, lexlist : List[LexUnit]) : Message = code match {
    case Protocol.make => new MakeMessage(lexlist)
    case Protocol.use => new UseMessage(lexlist)
    case Protocol.consume => new ConsumeMessage(lexlist)
    case Protocol.result => new ResultMessage(lexlist)
    case Protocol.self => new SelfMessage(lexlist)
    case Protocol.data => new DataMessage(lexlist)
    case Protocol.inventory => new InventoryMessage(lexlist)
    case Protocol.map => new MapMessage(lexlist)
    case c => throw ProtocolError("Code inconnu : " +c)
  }
}

package representation

import communication.parser._

/**
 * Résultats des actions.
 */
abstract sealed class Result(message: String) {
  val name = "Result"
  var description = message
  override def toString: String = {
    name + " : " + description
  }

  def lexem () : LexObject
}

object Result {
  def extract (lexem : LexUnit) : Result = lexem match {
    case lexobj : LexObject =>
      if (lexobj.getName != "RESULT")
	throw ProtocolError ("Ceci ne représente pas un Result : " +lexem)
      else {
	val content = lexobj.getContent
	if (content.length != 2)
	  throw ProtocolError("Result mal formé : " +lexem)
	val code = content(0).getIntValue
	val message = content(1).getStringValue
	code match {
	  case 0 => Success(message)
	  case 1 => Fail(message)
	  case 2 => Impossible(message)
	  case 3 => Error(message)
	  case _ => throw ProtocolError("Result mal formé : " +lexem)
	}
      }
    case _ => throw ProtocolError ("Ceci n'est pas un objet : " +lexem)
  }
}


/** Action effectuée avec succès. */
case class Success(message: String) extends Result(message) {
  override val name = "Success"
  override def lexem () : LexObject = {
    val name = new LexAtom("RESULT")
    val content = List(new LexInt(0), new LexString(message))
    new LexObject(name, content)
  }
}

/** Action possible, mais qui a échoué. */
case class Fail(message: String) extends Result(message) {
  override val name = "Fail"
  override def lexem () : LexObject = {
    val name = new LexAtom("RESULT")
    val content = List(new LexInt(1), new LexString(message))
    new LexObject(name, content)
  }
}

/** Action impossible à réaliser. */
case class Impossible(message: String) extends Result(message) {
  override val name = "Impossible"
  override def lexem () : LexObject = {
    val name = new LexAtom("RESULT")
    val content = List(new LexInt(2), new LexString(message))
    new LexObject(name, content)
  }
}

/** Action invalide. */
case class Error(message: String) extends Result(message) {
  override val name = "Error"
  override def lexem () : LexObject = {
    val name = new LexAtom("RESULT")
    val content = List(new LexInt(3), new LexString(message))
    new LexObject(name, content)
  }
}

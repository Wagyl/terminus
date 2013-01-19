package representation

import communication.parser._

/**
 * Résultats des actions.
 */
abstract sealed class Result(name : String, id : Int, message: String)
		extends Representation(name, id)
{
  var description = message

  override def toString: String = {
    name + " : " + description
  }

  def lexname () = "RESULT"
}



/** Action effectuée avec succès. */
case class Success (message: String) 
     extends Result("Success", Identifier.success, message)
{
  override def lexcontent () : List[LexUnit] = {
    List(new LexInt(0), new LexString(message))
  }
}

/** Action possible, mais qui a échoué. */
case class Fail (message: String) 
     extends Result("Fail", Identifier.fail, message)
{
  override def lexcontent () : List[LexUnit] = {
    List(new LexInt(1), new LexString(message))
  }
}

/** Action impossible à réaliser. */
case class Impossible(message: String)
     extends Result("Impossible", Identifier.impossible, message)
{
  override def lexcontent () : List[LexUnit] = {
    List(new LexInt(2), new LexString(message))
  }
}

/** Action invalide. */
case class Error (message: String)
     extends Result("Error", Identifier.error, message)
{
  override def lexcontent () : List[LexUnit] = {
    List(new LexInt(3), new LexString(message))
  }
}




object Result extends StreamCompanion[Result]  {
  def lexname () = "RESULT"

  def extract (content : List[LexUnit]) : Result = {
    if (content.length != 2)
      throw ProtocolError("Result mal formé : " +content)
    val code = content(0).getIntValue
    val message = content(1).getStringValue
    code match {
      case 0 => Success(message)
      case 1 => Fail(message)
      case 2 => Impossible(message)
      case 3 => Error(message)
      case _ => throw ProtocolError("Result mal formé : " +content)
    }
  }
}

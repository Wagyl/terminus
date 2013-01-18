package communication

import representation._

import scala.actors.Actor
import scala.actors.Actor._



trait Pending {

  var pendings : List[Receive] = List()

  def wait (nonce : Int) : Result = {
    val receive = new Receive(nonce)
    pendings = receive :: pendings
    receive.start
    val result = receive !? Ping
    pendings = pendings.filterNot(x => (x == receive))
    result match {
      case r : Result => r
      case _ => null
    }
  }
  
  def notify (nonce : Int, result : Result) : Unit = {
    val message = ResultReceipt(nonce, result)
    pendings.foreach(_.!(message))
  }
  
}


/*
 * Messages
 */

case class Ping ()

case class ResultReceipt (val nonce : Int, val result : Result)

/*
 * Actor
 */
class Receive (val id : Int) extends Actor {
  var res : Option[Result] = None
  var s : Option[scala.actors.OutputChannel[Any]] = None

  def end () : Unit = {
    if (s != None) {
      s.get ! ( res match { case Some(r) => r ; case None => null } )
    }
    exit()
  }

  def act () : Unit = {
    while (res == None) {
      receive {
	case Ping => 
	// println("PING : " +id)
	s = Some(sender)

	case ResultReceipt(nonce, result) if (nonce == id) =>
	// println("RESULT OK : " +id)
	res = Some(result)

	case ResultReceipt(nonce, result) if (nonce != id) =>
	// println("RESULT : " +id)
      }
    }

    while (s == None) {
      receive {
	case Ping => 
	// println("PING : " +id)
	s = Some(sender)
      }
    }

    end
  }
}

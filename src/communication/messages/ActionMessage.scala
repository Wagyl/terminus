package communication.messages

import communication.parser._
import communication._
import controller._
import representation._

/*
 * Messages du protocoles concernant les actions effectuées par le joueur.
 */

/* client -> serveur */

class MakeMessage (content : List[LexUnit]) extends ClientMessage(content) {
  override def process (controller : In, client : Connection) : Unit = {    
    val nonce = nth(1).getIntValue
    val action = nth(2).getIntValue
    /* Exécution de l'action */
    val result = controller.make(action)
    /* Envoi du result */
    val reply = ResultMessage(nonce, result)
    client.send(reply)
  }
}

object MakeMessage {
  def apply (nonce : Int, action : Int) : MakeMessage = {
    val lexnonce = LexInt(nonce)
    val lexaction = LexInt(action)
    val content = List[LexUnit](LexInt(Protocol.make), lexnonce, lexaction)
    new MakeMessage(content)
  }
}


class UseMessage (content : List[LexUnit]) extends ClientMessage(content) {
  override def process (controller : In, client : Connection) : Unit = {    
    val nonce = nth(1).getIntValue
    val entity = nth(2).getIntValue
    val action = nth(3).getIntValue
    /* Exécution de l'action */
    val result = controller.use(entity, action)
    /* Envoi du result */
    val reply = ResultMessage(nonce, result)
    client.send(reply)
  }
}

object UseMessage {
  def apply (nonce : Int, entity : Int, action : Int) : UseMessage = {
    val lexnonce = LexInt(nonce)
    val lexentity = LexInt(entity)
    val lexaction = LexInt(action)
    val lexcode = LexInt(Protocol.use)
    val content = List[LexUnit](lexcode, lexnonce, lexentity, lexaction)
    new UseMessage(content)
  }
}


class ConsumeMessage (content : List[LexUnit]) extends ClientMessage(content) {
  override def process (controller : In, client : Connection) : Unit = {    
    val nonce = nth(1).getIntValue
    val entity = nth(2).getIntValue
    val action = nth(3).getIntValue
    /* Exécution de l'action */
    val result = controller.consume(entity, action)
    /* Envoi du result */
    val reply = ResultMessage(nonce, result)
    client.send(reply)
  }
}

object ConsumeMessage {
  def apply (nonce : Int, entity : Int, action : Int) : ConsumeMessage = {
    val lexnonce = LexInt(nonce)
    val lexentity = LexInt(entity)
    val lexaction = LexInt(action)
    val lexcode = LexInt(Protocol.consume)
    val content = List[LexUnit](lexcode, lexnonce, lexentity, lexaction)
    new ConsumeMessage(content)
  }
}



/* serveur -> client */

class ResultMessage (content : List[LexUnit]) extends ServerMessage (content) {
  override def process (out : Out, pending : Pending, server : Connection) = {
    val nonce = nth(1).getIntValue
    val result = Result.extract(nth(2))
    pending.notify(nonce, result)
  }
}

object ResultMessage {

  def apply (nonce : Int, result : Result) : ResultMessage = {
    val lexresult = result.lexem
    val content = List[LexUnit](new LexInt(Protocol.result), LexInt(nonce),
      lexresult)
    new ResultMessage(content)
  }

}

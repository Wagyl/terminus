package representation

import data._
import communication.parser._

/*
 * Représentation des Évènements.
 */
abstract class EventInterface (name : String, identifier : Int) 
	 extends Representation (name, identifier) {
	val reason : Int
	val data : String
	var description = ""
}

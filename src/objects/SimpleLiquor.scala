package objects

import changes._
import representation._


class Fountain extends ConstEntity with Potable {

  val name = "Fountain"
  val description = "Fontaine."
  val identifier = Identifier.fountain


  val (l, m) = 
    if (scala.util.Random.nextInt(2) > 0)
      (List(new StaticLifeChange(-10)), "Vous buvez un poison.")
    else
      (List(new StaticLifeChange(10)),  "Vous buvez de l'eau.")
      
  val liquor : Liquor = l
  val drinkMessage : String = m
}


class Flask extends ConstEntity with Waterproof with Pickable {

  val name = "Flask"
  val description = "Bouteille."
  val identifier = Identifier.flask

}

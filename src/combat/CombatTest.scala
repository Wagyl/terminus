package combat

import objects._

object CombatTest {
  def main(args: Array[String]) {
    /* Un petit gobelin */
    val niceGob = Goblin.get()
    val wildGob = Goblin.get()

    println("wildGob hp : " + wildGob.health)

    Fight.handleFight(niceGob, wildGob, CompSpell)

    println("wildGob hp : " + wildGob.health)
  }
}

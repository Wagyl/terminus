package controller

/**
 * Ajout d'un pseudo temps-réel : tous les x millisecondes,
 * un certain nombre d'unité de temps
 * est accordé aux entités mutables.
 */
trait RealTime extends LocalGameControl {
  /** Le temps ajouté aux créatures à chaque tour. **/
  val time: Int

  /** Le temps entre chaque tour (millisecondes). **/
  val sleep: Int

  new Thread(new TurnThread()).start()

  class TurnThread extends Runnable {
    def run() = {
      while (true) {
        turn(time)
        Thread.sleep(sleep)
      }
    }
  }
}
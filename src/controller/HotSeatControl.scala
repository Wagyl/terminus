package controller

import objects._
import representation._
import data._

trait HotSeatControl extends LocalGameControl {
  /* Redéfinir game pour avoir un Game créé avec le trait HotSeatGame */
  override protected lazy val game: Game = Game.create(List(Identifier.hotSeat))

  protected var realGame: HotSeatGame = null

  game match {
    case x: HotSeatGame => realGame = x
    case _ => throw new ControllerException("Jeu de données incorrect pour l'extension HotSeat")
  }

  override def make(actionId: Int): Result = synchronized {
    val result = super.make(actionId)

    /* Après chaque action d'un joueur changer de joueur */
    val player = this.realGame.nextPlayer()

    /* Prévenir l'interface graphique du changement de joueur */
    this.out.event(new EventInterface("", Identifier.changePlayer) { val data = " Changement de joueur " + player.name + "."; val reason = Identifier.none })

    /* Envoyer les données pour le nouveau joueur à l'interface graphique */
    super.turn(0)

    result
  }
}
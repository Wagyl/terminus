package objects

import representation.EntityInterface
import representation.EventInterface


trait Memory extends Entity {

  private var _notices : List[Notice] = List()


  /*
   * La liste des notifications.
   */
  def notices : List[Notice] = _notices


  /*
   * Renvoie la liste des notifications actuelles
   * et vide la liste.
   */
  def getNotices : List[Notice] = {
    val rv = _notices
    _notices = List()
    rv
  }


  /*
   * Ajoute une notification à la liste.
   */
  def addNotice (identifier: Int, reason: Int, data: String) : Unit = {
    _notices = new Notice(identifier, reason, data) :: _notices
  }
  
  /*
   * Ajoute une notification à la liste.
   */
  def addNotice (notice: Notice) : Unit = {
    _notices = notice :: _notices
  }

}

class Notice(identifier: Int, reason: Int, data: String) {
  private val id = identifier
  private val _reason = reason
  private val _data = data
  
  def toEvent(): EventInterface = {
    val self = this
    new EventInterface("", this.id) { val data = self._data; val reason = self._reason }
  }
}

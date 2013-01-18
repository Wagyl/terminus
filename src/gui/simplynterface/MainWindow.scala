package gui.simplynterface

import controller._
import representation._
import data.Coordinates
import data.Floor
import data.Coordinates
import java.awt.Font
import scala.util.Properties
import representation.LandInterface
import representation.ContentInterface
import data.Tile
import scala.swing.Dialog
import javax.swing.ImageIcon

class MainWindow extends Out {
  var display: Display = new RealDisplay()
  var entry: Entry = new RealEntry()
  var init: InitControl = InitControl(this)
  var game: GameControl = null
  val playerId = -1
  var actionsTmp: List[((Int, Int), ActionInterface)] = Nil
  var lastResult = ""
  var lastEvent = ""
  var inInventory = false
  var extensions: List[Int] = Nil
  var gameOver: Boolean = false

  // Valeurs temporaire d'affichage
  var playerName: String = ""
  var character: SelfInterface = null
  var map: LandInterface = null
  var mapObjects: List[(Int, EntityInterface)] = Nil
  var inventory: List[(Int, ContentInterface)] = Nil

  def start(args: List[Int]) {
    this.extensions = args

    if (extensions.contains(Identifier.swing)) {
      val frame = new FrameDisplay()
      this.display = new SwingDisplay(frame)
      this.entry = new SwingEntry(frame.entry)
    }

    if (extensions.contains(Identifier.hari)) {
      val frame = new FrameDisplay()
      frame.display.font = new Font(Font.SANS_SERIF, Font.PLAIN, 14)
      this.display = new GraphicDisplay(frame)
      this.entry = new SwingEntry(frame.entry)
    }

    this.welcome()

    if (extensions.contains(Identifier.noIntro)) {
      this.game = this.init.createGame(0, 0, this.extensions)
      this.begin()
    } else {
      this.intro()
    }
  }

  def welcome() {
    this.display.printString("Bienvenue dans SimplYnterface v1.8.3 pour le Jeu <Insert Name Here>")
    this.display.printString("------------------------------------------------------------------")
    this.display.printString("------------------------------------------------------------------")
    this.display.printString("------------------------------------------------------------------")
  }

  def intro() {
    // Récupérer les informations de base du joueur 
    this.display.printString("Quel est votre sexe ?")
    val rep = entry.getRespons()

    if (rep.toLowerCase().equals("m")) {
      this.display.printString("Quel est votre nom l'aventurier ?")
    } else if (rep.toLowerCase().equals("f")) {
      this.display.printString("Quel est votre nom l'aventurière ?")
    } else {
      this.display.printString("Quel est votre nom le truc ?")
    }

    this.display.printString("Quel est votre nom ?")

    // Récupération du nom du joueur et création du jeu
    val char = this.init.createCharacter(0, entry.getRespons(), "", 0)
    this.game = this.init.createGame(0, char._1, this.extensions)
    this.playerName = char._2.name

    // Début du jeu
    this.begin()
  }

  def begin() {
    if (this.game == null) {
      this.display.printNewLine()
      this.display.printString("Erreur d'initialisation.")
      return
    }

    this.lastResult = "Bonjour " + this.playerName + "."
    this.refresh()

    var act = ""
    do {
      // Récupérer et executer l'action
      act = this.entry.getRespons()
      this.makeAction(act)
    } while (!act.equals("quit") && !this.gameOver)

    if (this.gameOver) {
      Dialog.showMessage(null, "Vous avez échoué dans votre quête de la Complétude Veritable. Soyez maudit !", "Game Over", Dialog.Message.Warning)
      Dialog.showMessage(null, "", "Game Over", Dialog.Message.Info, new ImageIcon("images/GO" + (scala.util.Random.nextInt(2) + 1) + ".jpg"))
    }

    this.display.quit()
    this.display.printString("Aurevoir " + this.character.name + ".")
  }

  def makeAction(act: String) {
    //TODO gérer les game over
    this.lastEvent = ""

    // Actions spécifique de l'interface graphique
    act match {
      case "i" ⇒
        this.inInventory = true
        this.entry.shortcut = false
        this.lastResult = ""
        this.refresh()
        return
      case "q" ⇒
        if (this.inInventory) {
          this.inInventory = false
          this.entry.shortcut = true
          this.lastResult = ""
          this.refresh()
          return
        }
      case "help" ⇒
        this.lastResult = (
          "Pour effectuer une action il vous suffit de taper son nom ou son numéro suivie de la touche entrer. Par exemple 'skip' ou '9'." + Properties.lineSeparator +
          "Vous pouvez également utiliser les flèches du clavier pour vous déplacer." + Properties.lineSeparator +
          "Ou utiliser l'une de ces autres commandes : " + Properties.lineSeparator +
          "i - Afficher l'inventaire." + Properties.lineSeparator +
          "q - Quitter l'inventaire." + Properties.lineSeparator +
          "desc - Afficher la description de l'objet le plus proche." + Properties.lineSeparator +
          "help - Afficher cette aide." + Properties.lineSeparator +
          "quit - Quitter l'application." + Properties.lineSeparator +
          "")
        this.refresh()
        return
      case "quit" ⇒ return
      case _ ⇒ ()
    }

    // Actions gérer par le controleur
    // Récupérer toutes les actions possibles
    val actions = this.actionsTmp

    // Trouver l'action correspondante
    var actOpt: Option[((Int, Int), ActionInterface)] = None

    // Chercher par ordre d'affichage
    if (this.isNumeric(act)) {
      val idx = act.toInt - 1
      if (idx >= 0 && idx < actions.size) {
        actOpt = Some(actions(idx))
      }
    }

    // Chercher par nom ou racourcie clavier
    if (actOpt.isEmpty && act.length() >= 4) {
      if (act.substring(0, 3).equals("/sh") && this.isNumeric(act.substring(3))) {
        val id = act.substring(3).toInt
        actOpt = actions.find(e ⇒ e._2.identifier == id)
      } else {
        actOpt = actions.find(e ⇒ e._2.name.toLowerCase().equals(act))
      }
    }

    // Executer l'action
    actOpt match {
      case None ⇒ this.lastResult = "Action inconnue." + this.lastEvent
      case Some((ids, act)) ⇒
        var res: Result = null
        if (this.inInventory) {
          res = this.game.consume(ids._2, ids._1)
        } else {
          if (ids._2 != playerId) {
            res = this.game.use(ids._2, ids._1)
          } else {
            res = this.game.make(ids._1)
          }
        }
        if (res != null) {
          this.lastResult = res.toString() + this.lastEvent
        } else {
          this.lastResult = "" + this.lastEvent
        }
    }

    this.refresh()
  }

  def getActions(): List[((Int, Int), ActionInterface)] = {
    // Déterminer la position du joueur TODO draft
    var playerX: Int = 0
    var playerY: Int = 0
    var playerZ: Int = 0
    this.character.position match {
      case Coordinates(x, y, Floor(z)) ⇒ playerX = x; playerY = y; playerZ = z
      case _ ⇒ ()
    }

    var list: List[((Int, Int), ActionInterface)] = List()

    if (this.inInventory) {
      // Actions sur les objets dans l'inventaire
      this.inventory foreach (
        elm ⇒ elm match {
          case (id, obj) ⇒ list :::= obj.usages.map(elm ⇒ ((elm._1, id), elm._2))
          case _ ⇒ ()
        })
    } else {
      // Actions relative à l'environnement
      this.mapObjects foreach (
        elm ⇒ elm match {
          case (id, obj) ⇒
            obj match {
              case obj: ObjectInterface ⇒
                val x = obj.position.x
                val y = obj.position.y
                val z = obj.position.z.z
                // Vérifier si l'objet est accessible au joueur //TODO clean/draft
                //if (this.isReachableFor(x, y, z, playerX, playerY, playerZ)) {
                list :::= obj.uses.map(elm ⇒ ((elm._1, id), elm._2))
              //}
              case _ ⇒ ()
            }
          case _ ⇒ ()
        })

      // Actions relative au joueur
      list :::= this.character.abilities.map(elm ⇒ ((elm._1, playerId), elm._2))
    }

    list
  }

  def isReachableFor(x: Int, y: Int, z: Int, ex: Int, ey: Int, ez: Int): Boolean = {
    if (z != ez) {
      false
    } else {
      val dx = x - ex
      val dy = y - ey
      (dx, dy) match {
        case (1, 1) | (1, -1) | (-1, 1) | (-1, -1) ⇒ true
        case _ ⇒ false
      }
    }
  }

  def getMapObjects(): List[ObjectInterface] = {
    var list = List[ObjectInterface]()
    val objects = this.mapObjects
    for ((id, oi) ← objects) {
      // Vérifier que ça soit bien un objet de la carte
      oi match {
        case o: ObjectInterface ⇒ list :::= List(o)

        /*// Vérifier qu'il soit bien sur la carte
          o.position match {
            case Coordinates(_, _, Floor(z)) => if (z == this.game.maps().floor) {
              // Ajouter l'objet à la fin de la liste
              list :::= List(o)
            }*/

        case _ ⇒ ()
        //}
      }
    }
    list
  }

  def getInventoryActions(): List[((Int, Int), ActionInterface)] = {
    // Déterminer la position du joueur TODO draft
    var playerX: Int = 0
    var playerY: Int = 0
    var playerZ: Int = 0
    this.character.position match {
      case Coordinates(x, y, Floor(z)) ⇒ playerX = x; playerY = y; playerZ = z
      case _ ⇒ ()
    }

    var list: List[((Int, Int), ActionInterface)] = List()

    // Actions relative à l'environnement
    this.mapObjects foreach (
      elm ⇒ elm match {
        case (id, obj) ⇒
          obj match {
            case obj: ObjectInterface ⇒
              val x = obj.position.x
              val y = obj.position.y
              val z = obj.position.z.z
              // Vérifier si l'objet est accessible au joueur //TODO
              //if (this.isReachableFor(x, y, z, playerX, playerY, playerZ)) {
              list :::= obj.uses.map(elm ⇒ ((elm._1, id), elm._2))
            //}
            case _ ⇒ ()
          }
        case _ ⇒ ()
      })

    // Actions relative au joueur
    list :::= this.character.abilities.map(elm ⇒ ((elm._1, playerId), elm._2))

    list
  }

  def isNumeric(input: String): Boolean = if (input == null || input.equals("")) false else input.forall(_.isDigit)

  override def character(data: (Int, SelfInterface)) {
    this.character = data._2
    this.playerName = data._2.name
    this.refresh()
  }

  //TODO
  def objects(data: List[(Int, EntityInterface)]) {
    this.mapObjects = data
    this.refresh()
  }

  def allObjects(data: List[((Int, TileInterface), List[(Int, EntityInterface)])]) {}

  def maps(data: LandInterface) {
    this.map = data
    this.refresh()
  }

  def inventory(data: List[(Int, ContentInterface)]) {
    this.inventory = data
    if (this.inInventory) {
      this.refresh()
    }
  }

  def refresh() {
    this.display.clear()

    // Afficher la carte
    if (this.map != null && this.character != null && this.mapObjects != null) {
      this.display.printMap(this.map, this.character, this.getMapObjects())
    }

    this.display.printString(this.lastResult)

    if (this.inInventory && this.inventory != null) {
      // Afficher l'inventaire
      this.display.printNewLine()
      this.display.printString("Taper q pour quitter l'inventaire.")
      this.display.printNewLine()
      this.display.printInventory(this.inventory)
    } else {
      this.display.printNewLine()
      this.display.printString("Taper i pour afficher l'inventaire.")
    }

    // Récupérer les actions possibles
    if (this.character != null && this.mapObjects != null) {
      this.actionsTmp = this.getActions()
    }

    // Afficher les actions possibles
    this.display.printActions(this.actionsTmp, this.inInventory)
    this.display.printNewLine()
    this.display.printString("Que voulez vous faire ?")

  }

  def event(data: EventInterface) {
    data.identifier match {
      case Identifier.gameEnd => data.reason match {
        case Identifier.playerDeath => this.lastEvent += sys.props("line.separator").toString() + "Fin du jeu, vous êtes mort !"; this.gameOver = true; this.refresh()
        case Identifier.disconnect => this.lastEvent += sys.props("line.separator").toString() + "Vous avez été déconnecté du serveur."; this.refresh()
        case _ =>
      }
      case Identifier.changePlayer => this.lastEvent += sys.props("line.separator").toString() + data.data; this.refresh()
      case Identifier.healthChange => data.reason match {
        case Identifier.hit => this.lastEvent += sys.props("line.separator").toString() + data.data; this.refresh()
        case _ => ()
      }
      case _ => ()
    }
  }
}

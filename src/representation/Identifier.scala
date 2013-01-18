package representation

/*
 * Index des identifiants des objets.
 */

object Identifier {

  /* CLASSES */

  val character = 0
  val warrior = 1
  val sorcerer = 2
  val rogue = 3

  /* CREATURES */

  val goblin = 1001
  val snake = 1002

  /* OBJETS */

  val doorOpen = 101
  val doorClosed = 102
  val fountain = 110

  val statue = 120
  val altar = 121

  /* */
  val flask = 201

  /* STUFF */
  val simpleHeadItem = 10001
  val simpleArmItem = 10002

  /* TILES */
  val indeterminate = 0
  val empty = 1
  val ground = 2
  val wall = 3
  val transparent_wall = 4

  /* ACTIONS */

  val moveAction = 10
  val moveTop = 11
  val moveBottom = 12
  val moveLeft = 13
  val moveRight = 14
  val moveTopLeft = 15
  val moveTopRight = 16
  val moveBottomLeft = 17
  val moveBottomRight = 18
  val moveActionMax = 18

  val openAction = 21
  val closeAction = 22

  val pickUpAction = 31
  val dropAction = 32

  val equip = 33
  val unequip = 34

  val drinkPotable = 40
  val drinkAction = 41

  val attack = 50

  val pray = 61
  val admire = 62

  /* EVENTS */
  val gameEnd = 0
  val changePlayer = 1
  val healthChange = 2

  /* REASONS */
  val none = 0
  val playerDeath = 1
  val disconnect = 2
  val hit = 3

  /* EXTENSIONS */
  val realTime = 1
  val hotSeat = 2
  val client = 3
  val server = 4

  val controlAction = 60
  val undoControlAction = 61

  /* AUTRES OPTIONS DE LANCEMENT */
  val noIntro = 70
  val hari = 71
  val swing = 72
}

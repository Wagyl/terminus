package changes

import objects._
import representation.Identifier

class PrayAction (a : Altar) extends Action {

  val altar : Altar = a
  var actor : Option[Entity with Humanoid] = None

  val name = "Pray"
  var description = "Prier..."
  val identifier = Identifier.pray

  val l1 = new Effect {
    val change = new StaticLifeChange(-10)
    def effector = actor.get
    val target = altar
  }
  val m1 = new Effect {
    val change = new BlankChange(1, "Vous dérangez le grand Giananas, qui entreprend alors de vous expliquer la transitivité conceptuelle. ")
    def effector = actor.get
    val target = altar
  }

  val l2 = new Effect {
    val change = new StaticLifeChange(+50)
    def effector = actor.get
    val target = altar
  }
  val m2 = new Effect {
    val change = new BlankChange(1, "Votre méditation vous permet d'atteindre la complétude véritable ! ")
    def effector = actor.get
    val target = altar
  }

  val e = new ConditionalBranch(Sequence(m1, l1), Sequence(m2, l2)) {
    def condition : Boolean = {
      scala.util.Random.nextInt(100) > 20
    }
  }

  addEffect(e)

  override def time (a : Entity) : Double = 1

  def assign (a : Entity) : (Boolean, String) = {
    a match {
      case x : Entity with Humanoid => actor = Some(x)
      case _ => return (false, "Vous ne pouvez pas prier.")
    }
    val reach = Action.reach(a, altar)
    if (!reach._1)
      return reach
    
    (true, "Vous priez en silence. ")
  }

  def abort () : Unit = { actor = None }

}



class AdmireAction (s : Statue) extends Action {

  val statue : Statue = s
  var actor : Option[Entity with Humanoid] = None

  val name = "Admire"
  var description = "Admirer la statue..."
  val identifier = Identifier.admire

  val l1 = new Effect {
    val change = new StaticLifeChange(+2)
    def effector = actor.get
    val target = statue
  }
  val m1 = new Effect {
    val change = new BlankChange(1, "il vous inspire courage. ")
    def effector = actor.get
    val target = statue
  }

  val l2 = new Effect {
    val change = new StaticLifeChange(-10)
    def effector = actor.get
    val target = statue
  }
  val m2 = new Effect {
    val change = new BlankChange(1, "il vous fait peur.  ")
    def effector = actor.get
    val target = statue
  }

  val e = new ConditionalBranch(Sequence(m1, l1), Sequence(m2, l2)) {
    def condition : Boolean = {
      scala.util.Random.nextInt(100) > 20
    }
  }

  addEffect(e)

  override def time (a : Entity) : Double = 1

  def assign (a : Entity) : (Boolean, String) = {
    a match {
      case x : Entity with Humanoid => actor = Some(x)
      case _ => return (false, "Vous ne pouvez pas admirer la statue.")
    }
    val reach = Action.reach(a, statue)
    if (!reach._1)
      return reach
    
    (true, "Vous admirez la statue de cet informaticien anonyme : ")
  }

  def abort () : Unit = { actor = None }

}

package log

trait Logable {
  def name : String
  def log : String

  def print (message : String) : Unit = Log.print(this, message)
  def warn (message : String) : Unit = Log.print(this, message)
  def error (message : String) : Unit = Log.print(this, message)
}

/* Il faudra certainement faire un truc plus malin qu'un object,
 * ceci n'est qu'une première approche */
object Log {
  def log(toLog: Logable) = println(toLog.name + " : " + toLog.log)
  def errLog(toLog: Logable) = System.err.println(toLog.name + " : " + toLog.log)

  def print (toLog : Logable, message : String) : Unit = {
    println("  | " +toLog.name + " ( " +toLog.log + " ) : " +message)
  }

  def warn (toLog : Logable, message : String) : Unit = {
    println("  | Warn de" +toLog.name + " ( " +toLog.log + " ) : " +message)
  }

  def error (toLog : Logable, message : String) : Unit = {
    println("  | ERROR de " +toLog.name + " ( " +toLog.log + " ) : " +message)
  }

}

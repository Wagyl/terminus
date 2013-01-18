package gui

import representation.Identifier

object ArgsParser {
  def parse(arg: String): Int = {
    arg match {
      case "-realtime" => Identifier.realTime
      case "-hotseat" => Identifier.hotSeat
      case "-client" => Identifier.client
      case "-server" => Identifier.server
      case "-hari" => Identifier.hari
      case "-nointro" => Identifier.noIntro
      case "-swing" => Identifier.swing
      case _ => -1
    }
  }
  def parse(args: Array[String]): List[Int] = {
    val res = for (a <- args) yield parse(a)
    res.toList
  }
}
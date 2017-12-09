package day9

import scala.io.Source

object Day9 {
  def main(args: Array[String]): Unit = {

    val line = Source.fromResource("problems/day9.txt").getLines() mkString

    var nestedLevel = 0
    var groupScore = 0
    var inGarbage = false
    var garbageIgnore = false
    var garbageCount = 0

    for (aktChar <- line) {
      if (!inGarbage) aktChar match {
        case '<' => inGarbage = true
        case '{' => nestedLevel += 1
        case '}' => {
          groupScore += nestedLevel
          nestedLevel -= 1
        }
        case _ =>

      } else {
        if (garbageIgnore) garbageIgnore = false
        else {
          aktChar match {
            case '!' => garbageIgnore = true
            case '>' => inGarbage = false
            case _ => garbageCount += 1
          }
        }
      }
    }

    println(f"Part 1: ${groupScore}")
    println(f"Part 2: ${garbageCount}")
  }
}

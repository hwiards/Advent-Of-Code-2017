package day11

import scala.io.Source

object Day11 {

  var maxDist = 0

  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("problems/day11.txt").getLines().toArray
    val moves = lines(0).split(",")

    println(f"Part 1: ${run(Position(), moves)}")
    println(f"Part 2: ${maxDist}")

  }

  def run(initPos: Position, moves: Array[String]): Int = {
    val endPos = moves.foldLeft(initPos)((pos, move) => pos.move(move))
    endPos.dist()
  }

  case class Position(val x: Int = 0, val y: Int = 0, val z: Int = 0) {

    def move(move: String): Position = {
      move match {
        case "s" => return Position(x, y - 1, z + 1).isGreater()
        case "se" => return Position(x + 1, y - 1, z).isGreater()
        case "ne" => return Position(x + 1, y, z - 1).isGreater()
        case "n" => return Position(x, y + 1, z - 1).isGreater()
        case "nw" => return Position(x - 1, y + 1, z).isGreater()
        case "sw" => return Position(x - 1, y, z + 1).isGreater()
      }
    }

    def dist(): Int = {
      (x.abs + y.abs + z.abs) / 2
    }

    def isGreater(): Position = {
      val aktDist = dist()
      if (maxDist < aktDist) maxDist = aktDist
      return this
    }
  }
}


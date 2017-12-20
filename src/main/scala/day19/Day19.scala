package day19

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day19 {

  val charList = ArrayBuffer[Char]()
  val down = Direction(1, 0)
  val left = Direction(0, -1)
  val up = Direction(-1, 0)
  val right = Direction(0, 1)

  def main(args: Array[String]): Unit = {
    val netzwerk = Source.fromResource("problems/day19.txt").getLines().toArray.map(line => line.toCharArray)

    def findStart(netWork: Array[Array[Char]]): State = {
      for (i <- netWork(0).indices) {
        if (netWork(0)(i) == '|') {
          return State(0, i, down, netzwerk)
        }
      }
      return State(-1, -1, down, netzwerk)
    }

    var aktState = findStart(netzwerk)

    var count = 0
    var end = false
    while (!end) {
      val nextState = aktState.run()
      if (nextState.x == -1) {
        end = true
      } else {
        aktState = nextState
      }
      count += 1
    }

    println(f"Part 1: ${charList.mkString}")
    println(f"Part 2: ${count}")

  }

  case class State(x: Int, y: Int, dir: Direction, netzwerk: Array[Array[Char]]) {

    def run(): State = {
      val aktSym = netzwerk(x)(y)

      aktSym match {
        case '+' => {
          dir match {
            case `down` | `up` => {
              if (isPath(left)) return newState(left)
              if (isPath(right)) return newState(right)
            }
            case `left` | `right` => {
              if (isPath(up)) return newState(up)
              if (isPath(down)) return newState((down))
            }
          }
        }
        case _ => {
          if (isPath(dir)) return newState(dir)
        }
      }
      return State(-1, -1, dir, netzwerk)
    }

    def newState(nextDir: Direction): State = {
      return State(x + nextDir.x, y + nextDir.y, nextDir, netzwerk)
    }

    def isPath(nextDir: Direction): Boolean = {

      val newX = x + nextDir.x
      val newY = y + nextDir.y
      if (!(netzwerk.length > newX && newX >= 0)) return false
      if (!(newY < netzwerk(newX).length) && newY >= 0) return false

      val nextSym = netzwerk(newX)(newY)

      if (nextSym.isLetter) {
        charList += nextSym
        return true
      }

      nextSym match {
        case '-' | '|' | '+' => true
        case _ => false
      }
    }
  }

  case class Direction(x: Int, y: Int)
}

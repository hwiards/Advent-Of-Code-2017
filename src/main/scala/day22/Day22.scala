package day22

import scala.collection.mutable.HashSet
import scala.io.Source

/*
 * Created by Hilko Wiards on 22.12.2017.
 */
object Day22 {
  
  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("problems/day22.txt").getLines().map(_.toCharArray).toArray

    val startDir = Direction(-1, 0)
    val startPos = Position(lines.length / 2, lines(0).length / 2)
    val startInfected = createInfectedList(lines)

    //Part 1
    var aktState = State(startPos, startDir, startInfected.clone(), HashSet[Position](), HashSet[Position](), 0)
    for (i <- 0 until 10000) {
      aktState = aktState.movePart1()
    }
    println(f"Part 1: ${aktState.countInfections}")


    //Part 2
    var aktState2 = State(startPos, startDir, startInfected.clone(), HashSet[Position](), HashSet[Position](), 0)
    for (i <- 0 until 10000000) {
      aktState2 = aktState2.movePart2()
    }
    println(f"Part 2: ${aktState2.countInfections}")
  }


  def createInfectedList(lines: Array[Array[Char]]): HashSet[Position] = {
    val posBuffer = HashSet[Position]()

    for (i <- lines.indices) {
      for (j <- lines(i).indices) {
        if (lines(i)(j) == '#') posBuffer += Position(i, j)
      }
    }
    return posBuffer

  }

  case class State(position: Position, direction: Direction, infected: HashSet[Position], weakened: HashSet[Position], flagged: HashSet[Position], countInfections: Int) {

    def movePart1(): State = {
      if (infected.contains(position)) {
        infected -= position
        val newDirection = direction.turnRight()
        val newPosition = position.move(newDirection)
        return State(newPosition, newDirection, infected, weakened, flagged, countInfections)
      } else {
        infected += position
        val newDirection = direction.turnLeft()
        val newPosition = position.move(newDirection)
        return State(newPosition, newDirection, infected, weakened, flagged, countInfections + 1)
      }
    }

    def movePart2(): State = {
      if (infected.contains(position)) {
        infected -= position
        flagged += position
        val newDirection = direction.turnRight()
        val newPosition = position.move(newDirection)
        return State(newPosition, newDirection, infected, weakened, flagged, countInfections)
      }
      if (weakened.contains(position)) {
        weakened -= position
        infected += position
        val newPosition = position.move(direction)
        return State(newPosition, direction, infected, weakened, flagged, countInfections + 1)
      }
      if (flagged.contains(position)) {
        flagged -= position
        val newDirection = direction.reverse()
        val newPosition = position.move(newDirection)
        return State(newPosition, newDirection, infected, weakened, flagged, countInfections)
      } else {
        weakened += position
        val newDirection = direction.turnLeft()
        val newPosition = position.move(newDirection)
        return State(newPosition, newDirection, infected, weakened, flagged, countInfections)
      }
    }

  }

  case class Position(x: Int, y: Int) {

    def move(dir: Direction): Position = {
      Position(x + dir.x, y + dir.y)
    }

  }

  case class Direction(x: Int, y: Int) {

    def turnRight(): Direction = {
      Direction(y, x * -1)
    }

    def turnLeft(): Direction = {
      Direction(y * -1, x)
    }

    def reverse(): Direction = {
      Direction(-x, -y)
    }
  }

}

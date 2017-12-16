package day16

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/*
 * Created by Hilko Wiards on 16.12.2017.
 */
object Day16 {

  val beginArray = "abcdefghijklmnop".toCharArray
  val begin = beginArray.to[ArrayBuffer]
  var counter = 0
  var cycle = -1

  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("problems/day16.txt").getLines().toArray

    //Part 1
    val arguments = lines(0).split(",")
    var end = arguments.foldLeft(begin)((state, argument) => parse(state, argument))
    println(f"Part 1: ${end.mkString}")


    //Part 2
    //Calc Cycle-Length
    val range = 1000000000
    var beginForCycleLen = beginArray.to[ArrayBuffer]
    part2cycleFind(beginForCycleLen, arguments, range)

    //Number of cycles really to perform
    val rest = (range % cycle)

    //Perform rest-cycles
    var beginPart2 = beginArray.to[ArrayBuffer]
    for (i <- 0 until rest) {
      beginPart2 = part1(beginPart2, arguments)
    }
    println(f"Part 2: ${beginPart2.mkString}")

  }

  def part1(state: ArrayBuffer[Char], arguments: Array[String]): ArrayBuffer[Char] = {
    arguments.foldLeft(state)((state, argument) => parse(state, argument))
  }

  def part2cycleFind(state: ArrayBuffer[Char], arguments: Array[String], range: Int): Unit = {
    var stateLocal = state
    for (i <- 0 until range) {
      stateLocal = part1(stateLocal, arguments)
      if (findCycle(stateLocal)) return
    }
  }


  def findCycle(state: ArrayBuffer[Char]): Boolean = {
    counter += 1
    if (state.toArray.deep == beginArray.deep) {
      cycle = counter
      return true
    }
    return false
  }

  def parse(state: ArrayBuffer[Char], argument: String): ArrayBuffer[Char] = {
    val arg1 = argument(0)
    arg1 match {
      case 's' => {
        val num = argument.drop(1).toInt
        return spin(state, num)
      }
      case 'x' => {
        val args = argument.drop(1).split("/").map(_.toInt)
        return exchange(state, args(0), args(1))
      }
      case 'p' => {
        val args = argument.drop(1).split("/")
        return partner(state, args(0)(0), args(1)(0))

      }
    }
  }


  def spin(state: ArrayBuffer[Char], amount: Int): ArrayBuffer[Char] = {
    val temp = state.clone()
    for (i <- 0 until state.length) {
      temp((i + amount) % state.length) = state(i)
    }
    return temp
  }

  def exchange(state: ArrayBuffer[Char], pos1: Int, pos2: Int): ArrayBuffer[Char] = {
    val temp = state(pos2)
    state(pos2) = state(pos1)
    state(pos1) = temp
    return state
  }

  //Transform to exchange
  def partner(state: ArrayBuffer[Char], ch1: Char, ch2: Char): ArrayBuffer[Char] = {
    val pos1 = state.indexOf(ch1)
    val pos2 = state.indexOf(ch2)
    return exchange(state, pos1, pos2)
  }
}

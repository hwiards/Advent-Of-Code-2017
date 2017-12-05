/*
 * Created by Hilko Wiards on 5.12.2017.
 */
package day5

import scala.io.Source

object Day5 extends App {

  val lines = Source.fromResource("problems/day5.txt").getLines().toArray
  val numArray = lines.map(_.toInt)

  println(f"Day 5, Part 1: ${prob1()}")
  println(f"Day 5, Part 2: ${prob2()}")

  //Part1
  def prob1(): Long = {
    // Clone since both Problems work on this Array[Int] and change values
    var nums = numArray.clone()

    var aktPos = 0
    var counter:Long = 0

    //Termination when jump out of "instruction list"
    while (aktPos < nums.length) {
      //calc next, setValue on old Pos, set new next ...
      val nextPos = aktPos + nums(aktPos)
      nums(aktPos) += 1
      aktPos = nextPos

      counter += 1 //increase counter
    }
    return counter
  }

  //Part2 Very similar to 1
  def prob2(): Long = {
    val nums = numArray.clone()
    var aktPos = 0
    var counter:Long = 0

    while (aktPos < nums.length) {
      val nextPos = aktPos + nums(aktPos)

      //Only change for part2
      if (nums(aktPos) >= 3) nums(aktPos) -= 1
      else nums(aktPos) += 1

      aktPos = nextPos
      counter += 1
    }
    return counter
  }
}
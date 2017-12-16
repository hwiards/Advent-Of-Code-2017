/*
 * Created by Hilko Wiards on 4.12.2017.
 */
package day04

import scala.io.Source

object Day4 extends App {

  //Import Lines (toStream to reuse these for Part2)
  val lines = Source.fromResource("problems/day4.txt").getLines().toStream
  val splitLines = lines.map(_.split("\\s"))

  //Part1
  //Count if the original size is equal to the size of the distinct list.
  val erg1 = splitLines.count(lineElems => lineElems.distinct.size == lineElems.size)
  println(f"Result Part 1: ${erg1}")


  // Part2
  //Go through every line and and sort every word in it. Now count like in Part1
  val sorted = splitLines.map(line => line.map(word => word.sorted))
  val erg2 = sorted.count(sortLines => sortLines.distinct.size == sortLines.size)
  println(f"Result Part 2: ${erg2}")


}
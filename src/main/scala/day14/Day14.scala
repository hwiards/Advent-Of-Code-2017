package day14

/*
 * Created by Hilko Wiards on 14.12.2017.
 */

import day10.Day10

import scala.collection.mutable.ArrayBuffer

object Day14 {

  var field: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer[ArrayBuffer[Int]]()
  var regions = 0

  def main(args: Array[String]): Unit = {

    val input = "ugkiagan"

    val i = (0 to 127).map(input + "-" + _)
    val j = i.map(Day10.hashBits(_))
    field = j.map(_.toCharArray.flatMap(_.asDigit.toBinaryString.reverse.padTo(4, '0').reverse.map(a => a.asDigit)).to[ArrayBuffer]).to[ArrayBuffer]

    val erg = field.toArray.map(_.sum).sum
    println(s"Part 1: $erg")
    println(s"Part 2: ${buildRegion()}")
  }


  def buildRegion(): Int = {

    while (true) {
      val start = findStart()
      if (start == (-1, -1)) {
        return regions
      }

      val group = ArrayBuffer[(Int, Int)](start)
      field(start._1)(start._2) = 0

      while (group.nonEmpty) {
        val aktPos = group.remove(0)
        // Haha..
        val posList = for {
          x <- (aktPos._1 - 1) to (aktPos._1 + 1)
          y <- (aktPos._2 - 1) to (aktPos._2 + 1)
          if (x == aktPos._1 && y != aktPos._2) || (x != aktPos._1 && y == aktPos._2)
          //ArrayOutOfBounds vermeiden :D
          a = Math.min(Math.max(0, x), field.length - 1)
          b = Math.min(Math.max(0, y), field.length - 1)
          pos = (a, b)
          if field(pos._1)(pos._2) == 1
        } yield pos

        //Alle besuchten Positionen auf 0 setzen.
        posList.foreach(pos => field(pos._1)(pos._2) = 0)
        group ++= posList

      }
      regions += 1
    }

    regions
  }

  def findStart(): (Int, Int) = {
    for {
      x <- 0 until field.length
      y <- 0 until field(0).length
      if field(x)(y) == 1
    } {
      return (x, y)
    }
    return (-1, -1)
  }

}
package day13

import scala.io.Source

object Day13 {

  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("problems/day13.txt").getLines().toArray

    val layerArray = for {
      line <- lines
      split = line.split(": ")
      pos = split(0).toInt
      depth = split(1).toInt

      layer = Layer(pos, depth)
    } yield layer

    println(f"Part 1: ${part1(0, layerArray)}")
    println(f"Part 2: ${part2(layerArray)}")

  }

  //+delay hinzugefügt um ein break an der Stelle
  def part1(delay: Int, layers: Array[Layer]): Int = {
    layers.filter(a => a.breaks(delay + a.pos)).map(_.impact()).sum
  }

  def part2(layers: Array[Layer]): Int = {
    var delay = 0
    while (layers.count(a => a.breaks(delay + a.pos)) != 0) delay += 1

    return delay
  }
}

case class Layer(val pos: Int, val depth: Int) {

  def breaks(time: Int): Boolean = {
    time % ((depth - 1) * 2) == 0
  }

  def impact(): Int = {
    pos * depth
  }

}
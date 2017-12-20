package day20

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day20 {

  def main(args: Array[String]): Unit = {
    val partikleStrings = Source.fromResource("problems/day20.txt").getLines().toArray
    val states = partikleStrings.map(parseLine(_)).to[ArrayBuffer]

    var break = 0
    var oldCount = states.size
    while(break < 500){
      states.transform(state => state.tick)
      val count = states.filter(!_.isDestroyed).filter(state => states.filter(!_.isDestroyed).count(_.equalPos(state))  == 1).size

      if(count == oldCount){
        break += 1
      }else{
        break = 0
        oldCount = count
      }

    }
    println(f"Part 1: ${states.indexOf(states.minBy(_.distance))}")
    println(f"Part 2: $oldCount")
  }

  def parseLine(line: String): State= {
    val split = line.split(">, ").map(_.replace(">",""))
    val vals = split.map(_.substring(3).split(",").map(_.toInt))

    val threeDims = vals.map( value => threeDim(value(0), value(1), value(2)))
    State(threeDims(0),threeDims(1), threeDims(2), false)
  }

  case class threeDim(x: Int, y: Int, z: Int){
    def add (plus: threeDim): threeDim = {
      threeDim(x + plus.x, y + plus.y, z + plus.z)
    }

    def equal(val2: threeDim): Boolean = {

      if (val2.x == x && val2.y == y && val2.z == z) {
        return true
      }
      return false

    }
  }

  case class State(p: threeDim, v: threeDim, a: threeDim, var isDestroyed: Boolean){
    def tick : State = {
      val newVel = v.add(a)
      val newPos = p.add(newVel)
      State(newPos, newVel, a, isDestroyed)
    }

    def equalPos(state: State): Boolean ={
      val isEq = p.equal(state.p)
      if (isEq && this != state) isDestroyed = true
      return isEq
    }

    def distance: Int = {
      p.x.abs + p.y.abs + p.z.abs
    }

  }

}

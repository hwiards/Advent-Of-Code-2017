package day12

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.io.Source

object Day12 {

  val programs: HashMap[Int, Program] = HashMap[Int, Program]()
  var inGroup: ArrayBuffer[Program] = ArrayBuffer[Program]()
  var numGroups = 0

  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("problems/day12.txt").getLines().toArray
    lines.foreach(line => parseLine(line))
    programs.values.foreach(_.setFollowers())
    inGroup += programs.get(0).get
    buildGroup()
    println(f"Part 1: ${inGroup.length}")
    countGroups()
    println(f"Part 2: ${numGroups}")

  }

  //Dirty, but works ;)
  //Delete all elements of current group.
  //Add the first element of remaining Program-Map
  //to the group and restart the build.
  def countGroups(): Unit = {
    while (true) {
      inGroup.foreach(prog => programs.remove(prog.ID))
      inGroup.clear()
      numGroups += 1

      //Check if Programs left after delete, else return.
      if (programs.values.isEmpty) return

      inGroup += programs.values.head
      buildGroup()
    }
  }

  def buildGroup(): Unit = {
    for (program <- inGroup) {
      for (follower <- program.followers) {
        if (!inGroup.contains(follower)) {
          inGroup += follower
          return buildGroup()
        }
      }
    }
  }

  def parseLine(line: String): Unit = {
    val split = line.split(" <-> ")
    val programID = split(0).toInt
    val followerIDs = split(1).split(", ").map(_.toInt)
    val program = Program(programID, followerIDs = followerIDs)
    programs.put(programID, program)
  }

  case class Program(val ID: Int, val followers: ArrayBuffer[Program] = ArrayBuffer(), followerIDs: Array[Int]) {
    def setFollowers(): Unit = followerIDs.foreach(followers += programs.get(_).get)
  }

}


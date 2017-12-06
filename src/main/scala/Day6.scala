/*
 * Created by Hilko Wiards on 6.12.2017.
 */
import scala.collection.mutable.HashMap
import scala.io.Source

object Day6 extends App {

  val line = Source.fromResource("problems/day6.txt").getLines().toArray
  var str = ""

  for (line <- line) str += line
  var nums = str.split("\\s").map(_.toInt)

  var counter = 0;
  var map: HashMap[Int, Array[Int]] = HashMap((counter, nums))

  val getKey: (Array[Int], HashMap[Int, Array[Int]]) => Int = (arr, hmap) => hmap.filter(tup => arr.sameElements(tup._2)).map(tup => tup._1).sum

  var loop = true
  while (loop) {
    var useInts = nums.clone();
    val highest = useInts.indexOf(useInts.max)
    var value = useInts(highest)
    useInts(highest) = 0
    val dist: Int = Math.ceil(value / (useInts.length.toFloat)).toInt


    var i = (highest + 1) % useInts.length
    while (value > 0) {
      if (value >= dist) {
        useInts(i) += dist
        value -= dist;
      } else {
        useInts(i) += value
        value = 0;
      }
      i = (i + 1) % useInts.length

    }
    counter += 1
    if (!map.values.exists(in => in.sameElements(useInts))) {
      map.put(counter, useInts)
      nums = useInts
    } else {

      var counterFirstOcc = getKey(useInts, map)

      println(f"Part1: ${counter}")
      println(f"Part2: ${counter} - ${counterFirstOcc} = ${counter - counterFirstOcc}")

      loop = false
    }
  }
}


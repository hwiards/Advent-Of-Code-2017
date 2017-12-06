package day6
/*
 * Created by Hilko Wiards on 6.12.2017.
 */
import scala.collection.mutable.HashMap
import scala.io.Source

object Day6 extends App {

  val line = Source.fromResource("problems/day6.txt").getLines().toArray
  var str = ""

  for (line <- line) str += line
  var bankStatus = str.split("\\s").map(_.toInt)

  var counter = 0;
  var statusMap: HashMap[Int, Array[Int]] = HashMap((counter, bankStatus))

  //Key zu übergebenen Value
  def getKey(arr: Array[Int], hmap: HashMap[Int, Array[Int]]):Int ={
    hmap.filter(tup => arr.sameElements(tup._2)).map(_._1).sum
  }


  var loop = true
  while (loop) {

    counter += 1
    var aktBankStatus = bankStatus.clone()

    //Höchster Wert-Index
    val highest = aktBankStatus.indexOf(aktBankStatus.max)
    var value = aktBankStatus(highest)
    aktBankStatus(highest) = 0
    //Wieviel wird pro Bank verteilt.
    val dist: Int = Math.ceil(value / (aktBankStatus.length.toFloat)).toInt

    //Starte bei Nachfolger der größten Bank
    var i = (highest + 1) % aktBankStatus.length
    while (value > 0) {
      aktBankStatus(i) += (if (value >= dist) dist else value)
      value -= dist;
      i = (i + 1) % aktBankStatus.length //Wrap around the banks

    }

    if (!statusMap.values.exists(status => status.sameElements(aktBankStatus))) {
      //Falls Status noch nicht enthalten, füge es zur Map hinzu.
      statusMap.put(counter, aktBankStatus)
      bankStatus = aktBankStatus

    } else {
      loop = false
      var counterFirstOcc = getKey(aktBankStatus, statusMap)

      println(f"Part1: ${counter}")
      println(f"Part2: ${counter} - ${counterFirstOcc} = ${counter - counterFirstOcc}")
    }
  }
}


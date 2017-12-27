package day21

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.io.Source

object Day21 {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("problems/day21.txt").getLines()
    val start = Source.fromResource("problems/day21-start.txt").getLines().toArray.map(_.toCharArray)


    val patternMap: HashMap[String, Array[Array[Char]]] = HashMap[String, Array[Array[Char]]]()

    lines foreach (line => {
      val split = line.split(" => ")
      val key = split(0).replaceAll("/", "")
      val value = split(1).split("/").map(_.toCharArray)
      patternMap.put(key, value)
    })

    var aktState = State(start, patternMap)
    for (count <- 1 to 18) {
      aktState = aktState.cycle()
      if (count == 5 || count == 18) {
        println(f"Number of Pixels on after $count Rounds: ${aktState.picture.map(_.count(_ == '#')).sum}")
      }
    }
  }

}

case class State(picture: Array[Array[Char]], patternMap: HashMap[String, Array[Array[Char]]]) {

  def cycle(): State = {

    val splitArray = split()
    val grown = lookup(splitArray)
    val whole = asWholeArray(grown)

    return State(whole, patternMap)

  }

  def split(): ArrayBuffer[ArrayBuffer[Array[Array[Char]]]] = {

    val size = if (isDiv2) 2 else 3

    val a = picture.map(_.grouped(size).toArray).grouped(size).toArray
    val b = a.map(row => row(0).indices.map(column => row.map(_ (column))).to[ArrayBuffer]).to[ArrayBuffer]

    return b
  }

  def isDiv2: Boolean = return picture.length % 2 == 0

  def asWholeArray(subArrays: ArrayBuffer[ArrayBuffer[Array[Array[Char]]]]): Array[Array[Char]] = {

    val subArraySize = subArrays(0)(0).size
    val size = subArrays.size * subArraySize

    val newArr = ArrayBuffer.fill[ArrayBuffer[Char]](size)(ArrayBuffer.fill[Char](size)(' '))

    for (i <- newArr.indices) {
      for (j <- newArr(0).indices) {
        newArr(i)(j) = subArrays(i / subArraySize)(j / subArraySize)(i % subArraySize)(j % subArraySize)
      }
    }

    return newArr.map(_.toArray).toArray

  }

  def lookup(array: ArrayBuffer[ArrayBuffer[Array[Array[Char]]]]): ArrayBuffer[ArrayBuffer[Array[Array[Char]]]] = {

    array.transform(arrayRow => arrayRow.transform(subArray => checkSub(subArray)))
  }

  def checkSub(subArray: Array[Array[Char]]): Array[Array[Char]] = {

    val subArraysList: ArrayBuffer[String] = ArrayBuffer[String]()

    subArraysList += asString(subArray)
    val trans1 = transpose(subArray)
    subArraysList += asString(trans1)
    val flip1 = flip(trans1)
    subArraysList += asString(flip1)
    val trans2 = transpose(flip1)
    subArraysList += asString(trans2)
    val flip2 = flip(trans2)
    subArraysList += asString(flip2)
    val trans3 = transpose(flip2)
    subArraysList += asString(trans3)
    val flip3 = flip(trans3)
    subArraysList += asString(flip3)
    val trans4 = transpose(flip3)

    for (keyString <- subArraysList) {
      if (patternMap.contains(keyString)) {
        return patternMap.get(keyString).get
      }
    }
    return subArray


  }

  def asString(subArray: Array[Array[Char]]): String = {
    var string = ""

    subArray foreach (row => string += row.mkString)

    return string

  }

  def transpose(subArray: Array[Array[Char]]): Array[Array[Char]] = {

    val newBuff = subArray.map(_.toBuffer).toBuffer
    for (i <- subArray.indices) {
      for (j <- subArray(0).indices) {
        newBuff(i)(j) = subArray(j)(i)
      }
    }
    return newBuff.map(_.toArray).toArray
  }

  def flip(subArray: Array[Array[Char]]): Array[Array[Char]] = {
    return subArray.map(_.reverse)
  }
}

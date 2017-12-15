package day15

/*
 * Created by Hilko Wiards on 15.12.2017.
 */
object Day15 {

  def main(args: Array[String]): Unit = {

    val startA = 634
    val startB = 301

    val multA = 16807
    val multB = 48271
    val modulo = 2147483647

    //Part 1
    val numVals1 = 40000000
    val iteratorA = getIterator(startA, multA, modulo)
    val iteratorB = getIterator(startB, multB, modulo)
    println(f"Part 1: ${run(iteratorA, iteratorB, numVals1)}")


    //Part 2
    val numVals2 = 5000000
    val iteratorA2 = getIterator(startA, multA, modulo).filter(_ % 4 == 0)
    val iteratorB2 = getIterator(startB, multB, modulo).filter(_ % 8 == 0)
    println(f"Part 2: ${run(iteratorA2, iteratorB2, numVals2)}")

  }

  def run(iterator1: Iterator[Int], iterator2: Iterator[Int], numVals: Int): Int = {
    iterator1.zip(iterator2).take(numVals).count(pair => (pair._1 & 0xFFFF) == (pair._2 & 0xFFFF))
  }


  def getIterator(start: Int, multVal: Int, modulo: Int): Iterator[Int] = {
    Iterator.iterate(start)(aktVal => ((aktVal.toLong * multVal) % modulo).toInt).drop(1)
  }
}

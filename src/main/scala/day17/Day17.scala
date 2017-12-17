package day17

import scala.collection.mutable.ArrayBuffer

/*
 * Created by Hilko Wiards on 17.12.2017.
 */
object Day17 {
  def main(args: Array[String]): Unit = {

    val input = 324
    val list = ArrayBuffer[Int](0)
    var aktPos = 0

    for(newVal <- 1 to 2017){
      aktPos = ((aktPos + input)%list.length +1 )
      list.insert(aktPos,newVal)
    }
    println(f"Part 1: ${list(aktPos+1)}")


    for(newVal <- 2018 to 50000000){
      aktPos = ((aktPos + input)%newVal +1 )
      if(aktPos == 1){
        list.insert(aktPos,newVal)
      }
    }
    println(f"Part 2: ${list(1)}")
  }
}
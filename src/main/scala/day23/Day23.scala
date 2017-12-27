package day23

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Exception.allCatch
import scala.io.Source

object Day23 {

  val register: ArrayBuffer[Long] = List.fill(8)(0.toLong).to[ArrayBuffer]
  var mulCount = 0

  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("problems/day23.txt").getLines().toArray

    register(0) = 1

    var pointer:Long = 0
    while(pointer < lines.length && pointer >= 0){
      pointer = parseLine(lines(pointer.toInt),pointer)
      println(register mkString(" "))
    }

    println(mulCount)


  }


  def parseLine(line: String, pointer: Long) : Long = {


    val split  = line.split(" ")
    split(0) match {
      case "sub" =>{
        val reg1 = split(1)(0) - 'a'
        if(isLongNumber(split(2))){
          register(reg1) -= split(2).toLong
        }else{
          val reg2 = split(2)(0) -'a'
          register(reg1) -= register(reg2)
        }
      }
      case "set" =>{
        val reg1 = split(1)(0) - 'a'
        if(isLongNumber(split(2))){
          register(reg1) = split(2).toLong
        }else{
          val reg2 = split(2)(0) -'a'
          register(reg1) = register(reg2)
        }
      }
      case "mul" => {
        val reg1 = split(1)(0) - 'a'
        mulCount += 1
        if (isLongNumber(split(2))) {
          register(reg1) *= split(2).toLong
        } else {
          val reg2 = split(2)(0) - 'a'
          register(reg1) *= register(reg2)
        }
      }
      case "jnz" => {
        if(isLongNumber(split(1))){
          if(split(1).toLong == 0) return pointer + 1
        }else{
          val reg1 = split(1)(0) - 'a'
          if(register(reg1) == 0){
            return pointer + 1
          }
        }
        if(isLongNumber(split(2))){
          return pointer + split(2).toLong
        }else{
          val reg2 = split(2)(0) - 'a'
          return pointer + register(reg2)
        }
      }
    }

    return pointer + 1
  }

  def isLongNumber(s: String): Boolean = (allCatch opt s.toLong).isDefined

}

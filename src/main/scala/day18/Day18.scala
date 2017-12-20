package day18

import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer
import scala.io.Source


// I don't want to look at this again.. :D
object Day18 {

  var sound: Long = 0
  val register: ArrayBuffer[Long] = List.fill(26)(0.toLong).to[ArrayBuffer]
  val lines = Source.fromResource("problems/day18.txt").getLines().toArray
  var send1 = 0

  def main(args: Array[String]): Unit = {

    val queue0 = Queue[Long]()
    val queue1 = Queue[Long]()


    // Part 1
    var pointer:Long = 0
    while(pointer < lines.length && pointer >= 0){
      pointer = parseLine(lines(pointer.toInt),pointer)
    }


    // Part 2
    val register0: ArrayBuffer[Long] = List.fill(26)(0.toLong).to[ArrayBuffer]
    val register1: ArrayBuffer[Long] = List.fill(26)(0.toLong).to[ArrayBuffer]
    var pointer0 = 0
    var pointer1 = 0

    val state0 = State(0, pointer0, queue0, queue1, register0).init()
    val state1 = State(1, pointer1, queue1, queue0, register1).init()



    var deadlock = false
    var prevPointer0 = 0
    var prevPointer1 = 0
    var prevSends0 = 0
    var prevSends1 = 0

    while(!deadlock){
      run(state0)
      run(state1)
      val pointer0 = state0.pointer
      val pointer1 = state1.pointer
      val send0 = state0.sends
      val send1 = state1.sends
      if((prevPointer0 == pointer0) && (prevPointer1 == pointer1) && prevSends0 == send0 && prevSends1 == send1) {
        deadlock = true
      }
      prevPointer0 = pointer0
      prevPointer1 = pointer1
      prevSends0 = state0.sends
      prevSends1 = state1.sends
    }
    println(f"Part 2: ${state1.sends}")
  }


  case class State (val id: Int, var pointer: Int, rcvQueue: Queue[Long], sendQueue: Queue[Long], register: ArrayBuffer[Long], var sends: Int = 0){
    def init(): State ={
      register('p'-'a') = id
      this
    }
  }

  def run(state: State): Int ={
    var blocked = false
    while(!blocked && state.pointer >=0 && state.pointer < lines.length){
      val erg = parseLine2(lines(state.pointer), state)
      state.pointer = erg._1
      blocked = erg._2
    }
    return state.pointer
  }


  def parseLine2(line: String, state: State) : (Int,Boolean) = {
    val split  = line.split(" ")
    split(0) match {
      case "snd" => {
        state.sends += 1
        if (split(1)(0).isLetter && split(1)(0) != '-') {
          state.sendQueue += state.register(split(1)(0) - 'a')
        } else {
          state.sendQueue += split(1).toLong
        }
      }
      case "add" => {
        val reg1 = split(1)(0) - 'a'
        if (split(2)(0).isLetter && split(2)(0) != '-') {
          val reg2 = split(2)(0) - 'a'
          state.register(reg1) += state.register(reg2)
        } else {
          val add = split(2).toInt
          state.register(reg1) += add
        }
      }
      case "set" => {
        val reg1 = split(1)(0) - 'a'
        if (split(2)(0).isLetter && split(2)(0) != '-') {
          val reg2 = split(2)(0) - 'a'
          state.register(reg1) = state.register(reg2)
        } else {
          val set = split(2).toInt
          state.register(reg1) = set
        }
      }
      case "mul" => {
        val reg1 = split(1)(0) - 'a'
        if (split(2)(0).isLetter && split(2)(0) != '-') {
          val reg2 = split(2)(0) - 'a'
          state.register(reg1) *= state.register(reg2)
        } else {
          val mult = split(2).toInt
          state.register(reg1) *= mult
        }
      }
      case "mod" => {
        val reg1 = split(1)(0) - 'a'
        if (split(2)(0).isLetter && split(2)(0) != '-') {
          val reg2 = split(2)(0) - 'a'
          state.register(reg1) = ((state.register(reg1) % state.register(reg2)) + state.register(reg2)) % state.register(reg2)
        } else {
          val mod = split(2).toInt
          state.register(reg1) = ((state.register(reg1) % mod) + mod) % mod
        }
      }
      case "rcv" => {
        if(state.rcvQueue.isEmpty){
          return (state.pointer,true)
        }
        val reg1 = split(1)(0) - 'a'
        state.register(reg1) = state.rcvQueue.dequeue()
      }
      case "jgz" => {
        if (split(1)(0).isLetter && split(1)(0) != '-') {
          val reg1 = split(1)(0) - 'a'
          if (state.register(reg1) > 0) {
            if (split(2)(0).isLetter && split(2)(0) != '-') {
              val offset = split(2)(0) - 'a'
              val regVal = state.register(offset)
              return ((state.pointer.toLong + regVal).toInt ,false)
            } else {
              val offset = split(2).toInt
              return (state.pointer + offset, false)
            }
          }
        } else {
          val check = split(1).toInt
          if (check > 0) {
            if (split(2)(0).isLetter && split(2)(0) != '-') {
              val offset = split(2)(0) - 'a'
              val regVal = state.register(offset)
              return ((state.pointer.toLong + regVal).toInt, false)
            } else {
              val offset = split(2).toInt
              return (state.pointer + offset, false)
            }
          }
        }
      }
    }
    return (state.pointer + 1, false)
  }

  def parseLine(line: String, pointer: Long) : Long = {
    val split  = line.split(" ")
    split(0) match {
      case "snd" => {
        if (split(1)(0).isLetter && split(1)(0) != '-') {
          sound = register(split(1)(0) - 'a')
        } else {
          sound = split(1).toInt
        }
      }
      case "add" => {
        val reg1 = split(1)(0) - 'a'
        if (split(2)(0).isLetter && split(2)(0) != '-') {
          val reg2 = split(2)(0) - 'a'
          register(reg1) += register(reg2)
        } else {
          val add = split(2).toInt
          register(reg1) += add
        }
      }
      case "set" => {
        val reg1 = split(1)(0) - 'a'
        if (split(2)(0).isLetter && split(2)(0) != '-') {
          val reg2 = split(2)(0) - 'a'
          register(reg1) = register(reg2)
        } else {
          val set = split(2).toInt
          register(reg1) = set
        }
      }
      case "mul" => {
        val reg1 = split(1)(0) - 'a'
        if (split(2)(0).isLetter && split(2)(0) != '-') {
          val reg2 = split(2)(0) - 'a'
          register(reg1) *= register(reg2)
        } else {
          val mult = split(2).toInt
          register(reg1) *= mult
        }
      }
      case "mod" => {
        val reg1 = split(1)(0) - 'a'
        if (split(2)(0).isLetter && split(2)(0) != '-') {
          val reg2 = split(2)(0) - 'a'
          register(reg1) = ((register(reg1) % register(reg2)) + register(reg2)) % register(reg2)
        } else {
          val mod = split(2).toInt
          register(reg1) = ((register(reg1) % mod) + mod) % mod
        }
      }
      case "rcv" => {
        if (split(1)(0).isLetter && split(1)(0) != '-') {
          val reg1 = split(1)(0) - 'a'
          if (register(reg1) != 0) {
            println(f"Recover: ${sound}")
            return -1
          }
        } else {
          if (split(1).toInt > 0) {
            println(f"Recover: ${sound}")
            return -1
          }
        }
      }
      case "jgz" => {
        if (split(1)(0).isLetter && split(1)(0) != '-') {
          val reg1 = split(1)(0) - 'a'
          if (register(reg1) > 0) {
            if (split(2)(0).isLetter && split(2)(0) != '-') {
              val offset = split(2)(0) - 'a'
              return pointer + register(offset)
            } else {
              val offset = split(2).toInt
              return pointer + offset
            }
          }
        } else {
          val check = split(1).toInt
          if (check > 0) {
            if (split(2)(0).isLetter && split(2)(0) != '-') {
              val offset = split(2)(0) - 'a'
              return pointer + register(offset)
            } else {
              val offset = split(2).toInt
              return pointer + offset
            }
          }
        }
      }
    }
    return pointer + 1
  }

}

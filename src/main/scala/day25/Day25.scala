package day25

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.io.Source

case class TuringState(write0: Int, dir0: Int, next0: Char, write1: Int, dir1: Int, next1: Char)

case class State(turState: TuringState, ones: HashSet[Int], pos: Int){

  def next(states: HashMap[Char, TuringState]): State ={
    if(!ones.contains(pos)){
      if(turState.write0 == 0) ones -= pos
      else ones += pos

      val newPos = pos + turState.dir0
      State(states.get(turState.next0).get, ones, newPos)
    }else{
      if(turState.write1 == 0) ones -= pos
      else ones += pos

      val newPos = pos + turState.dir1
      State(states.get(turState.next1).get, ones, newPos)
    }
  }
}

object Day25 {

  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("problems/day25.txt").getLines().toArray
    val start = lines(0)(15) - 'A'
    val diagnostic = lines(1).split("\\s")(5).toInt
    val states = HashMap[Char, TuringState]()

    //Parsing the States
    var startLine = 3
    while(startLine + 9 <= lines.length){
      val (name, state) = parseState(lines.slice(startLine, startLine + 9))
      states.put(name, state)
      startLine += 10
    }

    //Running the Machine
    val startState = State(states.get('A').get, HashSet[Int](), 0)
    val finalState = (1 to diagnostic).foldLeft(startState)((aktState, _ ) => aktState.next(states) )

    println(f"Final result 2017: ${finalState.ones.size}")

  }


  def parseState(stateLines: Array[String]):(Char, TuringState) = {

    val name = stateLines(0)(9)
    val write0 = stateLines(2)(22).asDigit
    val dir0 = if (stateLines(3)(27) == 'r') 1 else -1
    val next0 = stateLines(4)(26)
    val write1 = stateLines(6)(22).asDigit
    val dir1 = if (stateLines(7)(27) == 'r') 1 else -1
    val next1 = stateLines(8)(26)

    (name, TuringState(write0, dir0, next0, write1, dir1, next1))
  }

}

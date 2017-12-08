/*
 * Created by Hilko Wiards on 8.12.2017.
 */

import scala.collection.mutable
import scala.io.Source

object Day8 {
  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("problems/day8.txt").getLines().toArray

    var registers: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int]()
    var max = 0

    //Walk throug all lines of the Program
    for (progLine <- lines) {
      //Assign the values
      val split = progLine.split("\\s")
      val register = split(0)
      val operation = split(1)
      val changeValue = split(2).toInt
      val condValue = split(6).toInt
      val condition = split(5)
      val condRegister = split(4)

      //If the two registers do not exist yet, create them
      if (registers.get(register).isEmpty) {
        registers.put(register, 0)
      }
      if (registers.get(condRegister).isEmpty) {
        registers.put(condRegister, 0)
      }

      //Match "inc" and "dec"
      operation match {
        case "inc" => IncrementIf(register, changeValue, condRegister, condition, condValue)
        case "dec" => DecrementIf(register, changeValue, condRegister, condition, condValue)
      }

      if (registers.get(register).get > max) max = registers.get(register).get

    }

    println(f"Part1: ${registers.values.max}")
    println(f"Part2: ${max}")


    //Increment if condition is met.
    def IncrementIf(register: String, changeValue: Int, condRegister: String, condition: String, condValue: Int): Unit = {
      if (checkCond(condRegister, condition, condValue)) {
        registers(register) += changeValue
      }
    }

    //Decrement if condition is met.
    def DecrementIf(register: String, changeValue: Int, condRegister: String, condition: String, condValue: Int): Unit = {
      if (checkCond(condRegister, condition, condValue)) {
        registers(register) -= changeValue
      }
    }

    //Check condition.
    def checkCond(condRegister: String, condition: String, condValue: Int): Boolean = {

      condition match {
        case "==" => return registers.get(condRegister).get == condValue
        case ">=" => return registers.get(condRegister).get >= condValue
        case ">" => return registers.get(condRegister).get > condValue
        case "<=" => return registers.get(condRegister).get <= condValue
        case "<" => return registers.get(condRegister).get < condValue
        case "!=" => return registers.get(condRegister).get != condValue
      }

    }
  }
}


/*
 * Created by Hilko Wiards on 8.12.2017.
 */

import scala.collection.mutable
import scala.io.Source

object Day8 extends App {

  val lines = Source.fromResource("problems/day8.txt").getLines().toArray
  var registers: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int]()
  var max = 0

  for (progLine <- lines) {
    val split = progLine.split("\\s")
    val register = split(0)
    val changeValue = split(2).toInt
    val condValue = split(6).toInt
    val condition = split(5)
    val condRegister = split(4)

    if (registers.get(register).isEmpty) {
      registers.put(register, 0)
    }
    if (registers.get(condRegister).isEmpty) {
      registers.put(condRegister, 0)
    }

    if (split(1) == "inc") {
      IncrementIf(register, changeValue, condRegister, condition, condValue)
    } else {
      DecrementIf(register, changeValue, condRegister, condition, condValue)
    }

    if (registers.get(register).get > max) max = registers.get(register).get
  }

  println(f"Part1: ${registers.values.max}")
  println(f"Part2: ${max}")

  def IncrementIf(register: String, changeValue: Int, condRegister: String, condition: String, condValue: Int): Unit = {

    if (checkCond(condRegister, condition, condValue)) {
      val oldValue = registers.get(register).get
      val newValue = oldValue + changeValue
      registers.put(register, newValue)
    }

  }

  def DecrementIf(register: String, changeValue: Int, condRegister: String, condition: String, condValue: Int): Unit = {
    if (checkCond(condRegister, condition, condValue)) {
      val oldValue = registers.get(register).get
      val newValue = oldValue - changeValue
      registers.put(register, newValue)
    }
  }

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


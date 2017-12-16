/*
 * Created by Hilko Wiards on 1.12.2017.
 */
package day01

import scala.io.Source

object Day1 extends App {

  val file = Source.fromResource("problems/day1.txt").getLines()
  var str: String = ""

  for (line <- file) str += line
  str.filterNot(x => x.isWhitespace) //Whitespace, Endline etc entfernen...

  //Berechnungsfuntion
  def calcDay1(str: String, off: Int): Int = {
    Range(0, len) //Erhalte die Zahlen von 0 bis n-1
      .filter(i => str(i) == str((i + off) % len)) // Fuer jede Zahl entscheide ob nächste Stelle gleich ist
      .map(str(_).asDigit) //char zu int. _ als Platzhalter.
      .sum //Summe
  }

  val len = str.length
  println(calcDay1(str, 1))
  println(calcDay1(str, len / 2))
}
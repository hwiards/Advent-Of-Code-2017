/*
 * Created by Hilko Wiards on 1.12.2017.
 */
package day01

import scala.io.Source

object Day2 extends App {

  val file: Iterator[String] = Source.fromResource("problems/day2.txt").getLines()
  var str: String = ""

  for (line <- file) {
    str += line
    str += "\n"
  }

  //Berechnungsfuntion Aufgabe 1
  def calcDay2_1(str: String): Int = {
    val linesRows = str .lines
                        .map(_.split("\\s") //Zeilen am Whitespace trennen.
                        .map(Integer.parseInt(_))) //Aus String einen Int machen
                        .toList // Liste

    linesRows.map(zeile => zeile.max - zeile.min).sum
  }

  println(calcDay2_1(str))

  //Berechnungsfunktion Aufgabe2
  def calcDay2_2(str: String): Int = {
    // Wie bei 1.
    val linesRows = str .lines
                        .map(_.split("\\s") //Zeilen am Whitespace trennen.
                        .map(Integer.parseInt(_))) //Aus String einen Int machen
                        .toList // Liste

    val pairs = linesRows.map(zeile => zeile.combinations(2))  //Erstelle alle 2er Kombinationen der Elemente der Zeile

    pairs.flatMap(zeile => zeile
                            .filter(paar => paar.max % paar.min == 0)
                            .map(paar => paar.max/paar.min)).sum
  }

  println(calcDay2_2(str))
}
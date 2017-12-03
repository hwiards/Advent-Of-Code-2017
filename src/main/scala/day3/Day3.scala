/*
 * Created by Hilko Wiards on 3.12.2017.
 */

//Included both problems in one program since the values in the field are not needed for problem 1.
object Day3 extends App {

  val input = 289326
  val sidelength = Math.ceil(Math.sqrt(input)).asInstanceOf[Int]
  val half = if (sidelength % 2 == 0) (sidelength + 1) / 2 else sidelength / 2

  var nums = Array.ofDim[Int](sidelength + 2, sidelength + 2)

  var dir = 3 //Up 0, Left 1, Down 2, Right 3
  var x = half
  var y = half
  var aktMaxDist = 0 //Distance from Center
  var isErg2 = false

  //Functions
  val square: Int => Int = i => i * i
  val getSquareToDist: Int => Int = i => square((i * 2) + 1)
  val dist: (Int, Int, Int) => Int = (a, b, to) => (a - to).abs + (b - to).abs

  for (num <- 1 to input) {

    nums(x)(y) = calcNumValue(x, y)
    if (num == 1) nums(x)(y) = 1

    if (nums(x)(y) > input && !isErg2) {
      println("Erg 2: " + nums(x)(y))
      isErg2 = true
    }
    if (num == input) println("Erg 1: " + dist(x, y, half))

    //Match the Direction to calculate the next coordinate
    //Test if the actual "circle" around the center is not left
    //Change direction if it is left.
    //In the lower right corner it should leave the circle..
    dir match {
      case 0 =>
        if (Math.abs((y - half) + 1) <= aktMaxDist) {
          y += 1
        } else {
          dir = (dir + 1) % 4
          x -= 1
        }
      case 1 =>
        if (Math.abs((x - half) - 1) <= aktMaxDist) {
          x -= 1
        } else {
          dir = (dir + 1) % 4
          y -= 1
        }
      case 2 =>
        if (Math.abs((y - half) - 1) <= aktMaxDist)
          y -= 1
        else {
          dir = (dir + 1) % 4
          x += 1
        }
      case 3 =>
        if (Math.abs((x - half) + 1) <= aktMaxDist)
          x += 1
        else {
          dir = (dir + 1) % 4
          x += 1 //Sonderfall, da wir hier aus dem Kreis rauslaufen
        }
    }

    if (num + 1 > getSquareToDist(aktMaxDist)) aktMaxDist += 1

  }

  //Calculate the values for Exercise 2
  def calcNumValue(X: Int, Y: Int): Int = {

    var sum = 0
    for {
      i <- X - 1 to X + 1
      j <- Y - 1 to Y + 1
      if i >= 0 && i < nums.length
      if j >= 0 && j < nums.length
      if i != X || j != Y
    } {
      sum += nums(i)(j)
    }
    return sum
  }

}

package day23

object Day23_2 {

  def main(args: Array[String]): Unit = {

    println(run())
  }

  def run(): Int ={
    var a = 1
    var b = 0
    var c = 0
    var d = 0
    var e = 0
    var f = 0
    var g = 0
    var h = 0

    b = 99
    c = b
    b *= 100
    b += 100000
    c = b
    c += 17000

    do {
      f = 1
      d = 2

      //println(a + " " +b + " " +c + " " +d + " " +e + " " +f + " " +g + " " +h)

      do {
        e = 2

        if(b % d == 0){
          f = 0
        }

        // A bad way to calculate b%d
//        do {
            //Multipliziere d so oft, mit einem größer werdenen e,
            //bis man wenn man b abzieht auf 0 kommt.
            //Dann muss d ein Vielfaches von b gewesen sein. also b%d == 0
//          g = d
//          g *= e
//          g -= b
//
//          if (g == 0) {
//            f = 0
//          }
//
//          e += 1 // Erhöhe e
            //Teste ob e kleiner als b, wenn ja Abbruch
//          g = e
//          g -= b
//        } while (g != 0)

        d += 1
        g = d
        g -= b
      } while (g != 0)

      if (f == 0) {
        h += 1
      }
      g = b
      g -= c

      if (g == 0) {
        return h
      }
      b += 17

    }while(true)

    return h
  }
}
package day10

object Day10 {

  val stdSuffix:Array[Int] = Array(17, 31, 73, 47, 23)

  def main(args: Array[String]): Unit = {

    val input = "88,88,211,106,141,1,78,254,2,111,77,255,90,0,54,205"


    val lenghtInput = input.split(",").map(_.toInt)
    val charInput = input.toCharArray.map(_.toInt)
    val totalChars = charInput ++ stdSuffix

    val initialState = State()
    println(f"Part 1: ${process(initialState,lenghtInput).productOfFirst(2)}")
    println(f"Part 2: ${hex(xor(processTimes(initialState,charInput, 64)))}")
  }

  def hashBits(input:String): String ={
    hex(xor(processTimes(State(),input.toCharArray.map(_.toInt), 64)))
  }

  //Aktueller Status
  case class State (nums: Vector[Int] = Vector.range(0,256), pos:Int = 0, skipsize:Int = 0){

    def productOfFirst(len: Int):Int = nums.take(len).product

    def run(len: Int): State ={
      val temp = nums.toBuffer
      for(i <- 0 until len){
        temp((pos+i) % temp.length) = nums((pos+len-i-1)%nums.length)
      }
      State(temp.toVector, (pos + len + skipsize) % nums.length, skipsize + 1)
    }
  }

  def process (state:State, lenghts: Array[Int]) : State ={
    //foldLeft geht durch alle Längen und ruft die run() von State auf
    //Das Ergebnis von run() wird nun für die neue length verwendet.
    lenghts.foldLeft(state)((sta,len) => sta.run(len))
  }

  //Durchläuft den Prozess repeat mal.
  def processTimes(state:State, input: Array[Int], repeat:Int) : State = {

    val lenghts = input++stdSuffix

    //Geht bestimmt auch eleganter..
    var aktState = state
    for(round <- 0 until repeat){
      aktState = process(aktState, lenghts)
    }
    aktState
  }

  //XORt jeweils 16 Werte.
  def xor(state:State):Array[Int] ={
    state.nums.grouped(16).map(vec => vec.reduce((num1, num2) => num1 ^ num2)).toArray
  }

  //Erstellt einen Hex-String
  def hex(nums: Array[Int]):String ={
    //reverse.padTo(2,'0').reverse da nicht alle Zahlen zweistellig
    nums.map(num => num.toHexString.reverse.padTo(2,'0').reverse).mkString
  }

}


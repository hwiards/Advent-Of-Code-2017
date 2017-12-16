/*
 * Created by Hilko Wiards on 7.12.2017.
 */
import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import scala.io.Source

/*
 *  Not the nicest solution, but it worked for now ..
 */

object Day7 extends App {

  val lines = Source.fromResource("problems/day7.txt").getLines().toArray

  //FindRoot
  var programms: HashMap[String, Program]  = new HashMap[String, Program]()

  val root = findRoot(lines)
  println(root.name)
  calcWeights(root)
  balance(root,false)



  def balance (root:Program, nextToo:Boolean): Unit ={
    val numFollowers = root.followers.size
    var nextTooCall = false
    if(numFollowers > 0){
      val weightList = root.followers.map(follower => follower.totWeight)
      if(weightList.max != weightList.min){
        println(weightList)
        nextTooCall = true
      }else if (nextToo) {
        println(weightList)
      }
      root.followers.foreach(follower => balance(follower,nextTooCall))
    }
  }


  def calcWeights(root:Program):Int = {
    for(follower <- root.followers){
      root.totWeight += calcWeights(follower)
    }
    root.totWeight += root.weight
    return root.totWeight

  }


  def createProgram(programString: String): Program = {

    val arrowSplit: Array[String] = programString.split("->")
    val _name: String = arrowSplit(0).split("\\s")(0)
    val _weight: Int = arrowSplit(0).split("\\s")(1).replaceAll("\\(","").replaceAll("\\)","").toInt

    val _followerString =  if(arrowSplit.length == 2) arrowSplit(1).split(", ").map(_.replaceAll("\\s","")) else new Array[String](0)


    return  new Program( name = _name, weight = _weight, followerStrings = _followerString)
  }


  def findRoot(programStrings: Array[String]):Program = {

    var subprogramms:ListBuffer[Program] = new ListBuffer[Program]()

    for(progLine <- programStrings){
      val program = createProgram(progLine)
      programms.put(program.name, program)
    }

    for((name, program) <- programms){
      for(subProgramString <- program.followerStrings){
        val subProgram = programms.get(subProgramString)
        if(!subProgram.isEmpty){
          program.followers.append(subProgram.get)
          subprogramms.append(subProgram.get)
        }
      }
    }

    programms.values.find(progr => !subprogramms.contains(progr)).get



  }








  class Program (val name: String, val weight: Int, var totWeight: Int = 0, val followerStrings: Array[String], var followers:ArrayBuffer[Program] = new ArrayBuffer[Program]()){

    def addFollower(follower: Program):Unit ={
      followers.append(follower)
    }

    def calcTotalWeight():Int = {
      totWeight = weight
      for(follower <- followers){
        totWeight += follower.calcTotalWeight()
      }
      return totWeight
    }

  }

}


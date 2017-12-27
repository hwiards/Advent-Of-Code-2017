package day24

import scala.io.Source

object Day24 {

  case class Component (val port1: Int, val port2: Int)

  case class Bridge(components: List[Component], aktPort: Int, strength: Int){

    def connect(comp: Component): Option[Bridge] = comp match {
      case comp if(comp.port1 == aktPort) => Some(Bridge(comp :: components, comp.port2, strength + comp.port1 + comp.port2))
      case comp if(comp.port2 == aktPort) => Some(Bridge(comp :: components, comp.port1, strength + comp.port1 + comp.port2))
      case _ => None
    }
  }

  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("problems/day24.txt").getLines().map(_.split("/").map(_.toInt)).toArray
    val components = lines.map(compArray => Component(compArray(0), compArray(1)))
    val componentsSet = components.toSet

    val part1 = Bridges(Bridge(List[Component](), 0, 0), componentsSet)
    val part2 = Bridges(Bridge(List[Component](), 0, 0), componentsSet).map(bridge => (bridge.components.length, bridge.strength)).max._2
    println(f"Part 1: ${part1.map(_.strength).max}")
    println(f"Part 2: $part2")

  }

  def Bridges(bridge: Bridge, components: Set[Component]): Iterator[Bridge] ={

    val temp = components.toIterator.flatMap(bridge.connect)
    if(temp.isEmpty) {
      return Iterator(bridge)
    }else {
      return temp.flatMap(a => Bridges(a, components - a.components.head))
    }


  }
}

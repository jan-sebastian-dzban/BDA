import scala.collection.mutable.Map
import scala.io.Source

object GraphAnalysis {
    def mapFunction(line: String): (Int, Int) = {
        val l = line.split("\\s").map(x => x.toInt)
        return (l(0), l(1))
    }

    def reduceFunction(map: Map[Int, (Int, Int)], edge: (Int, Int)): Map[Int, (Int, Int)] = {
        val (from, to) = edge
        val (fromInCount, fromOutCount) = map.getOrElse(from, (0, 0))
        val (toInCount, toOutCount) = map.getOrElse(to, (0, 0))

        map.put(from, (fromInCount, fromOutCount + 1))
        map.put(to, (toInCount + 1, toOutCount))

        return map
    }

    def main(args: Array[String]): Unit = {
        val input = args(0)
        println(Source.fromFile(input).getLines().map(mapFunction).foldLeft(Map[Int, (Int, Int)]())(reduceFunction))
    }
}
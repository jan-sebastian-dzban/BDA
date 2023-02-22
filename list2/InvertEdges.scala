import scala.collection.mutable.Map

object InvertEdges {
    val input = List(
        (1, List(2,3)),
        (3, List(1, 5)),
        (2, List(5)),
        (5, List())
    )
    
    def map(pair: (Int, List[Int])): (Int, List[Int]) = {
        pair
    }

    def reduce(edges: List[(Int, List[Int])]) = {
        val result = Map[Int, List[Int]]()
        
        edges.foreach(edge => {
            val (node, neighbors) = edge
            neighbors.foreach(neighbor => {
                result.get(neighbor) match {
                    case Some(list) => result.put(neighbor, node :: list)
                    case None => result.put(neighbor, List(node))
                }
            })
        })

        result.toList
    }


    def main(args: Array[String]): Unit = {
        val result = reduce(input)

        println(result)
    }
}
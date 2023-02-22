import scala.io.Source

object DocumentsSimilarity {
    def kshingles(k: Int, book: String): Set[String] = {
        val words = book.replaceAll("""[“”\p{Punct}]""", "").toLowerCase.split("\\s+")
        val kshingles = words.sliding(k).map(_.mkString(" ")).toSet
        kshingles
    }

    def jaccard(a: Set[String], b: Set[String]): Double = {
        val intersection = a.intersect(b)
        val union = a.union(b)
        intersection.size.toDouble / union.size.toDouble
    }

    def main(args: Array[String]): Unit = {
        val books = args.map(Source.fromFile(_).mkString).toList

        for (k <- 4 to 13) {
            println(s"$k-shingles:")
            val ks = books.map(kshingles(k, _))
            for (i <- 0 until books.size; j <- i + 1 until books.size) {
                println(s"Between ${args(i)} and ${args(j)}: ${jaccard(ks(i), ks(j))}") 
            }
        }
    }
}
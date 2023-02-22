import scala.io.Source
import scala.util.Random

object Minhashing {
    def kshingles(k: Int, book: String): Set[String] = {
        val words = book.replaceAll("""[“”\p{Punct}]""", "").toLowerCase.split("\\s+")
        val kshingles = words.sliding(k).map(_.mkString(" ")).toSet
        kshingles
    }

    def signature(shingles: Set[String], hashingFuns: Array[(String) => Int]): Set[Int] = {
        hashingFuns.map(fn => shingles.map(fn).min).toSet
    }

    def jaccard(s1: Set[Int], s2: Set[Int]): Double = {
        val intersection = s1.intersect(s2)
        val union = s1.union(s2)
        intersection.size.toDouble / union.size.toDouble
    }

    def main(args: Array[String]): Unit = {
        val books = args.map(Source.fromFile(_).mkString).toList

        for (h <- List(10, 100, 250, 500)) {
            val hashingFns = (0 until h)
                .map((_) => (Random.nextInt(), Random.nextInt()))
                .map((a) => (s: String) => ((a._1 * s.hashCode.abs + a._2) % Int.MaxValue))
                .toArray
            println(s"$h hashes:")
            for (k <- 4 to 13) {
                println(s"$k-shingles:")
                val ks = books.map(kshingles(k, _))
                val signatures = ks.map(signature(_, hashingFns))
                for (i <- 0 until books.size; j <- i + 1 until books.size) {
                    println(s"Between ${args(i)} and ${args(j)}: ${jaccard(signatures(i), signatures(j))}") 
                }
            }
        }

    }
}
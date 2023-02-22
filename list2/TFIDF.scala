import java.io._
import scala.io.Source
import scala.collection.mutable.Map
import scala.io.StdIn.readLine
import scala.util.{Try,Success,Failure}

class TFIDF {
    private val _stopWords: List[String] = Source.fromFile("../data/stop_words_english.txt").getLines.toList
    private val _wordCounts: Map[String, Map[String, Int]] = Map()

    def parseFile(file: File): Unit = {
        val source = Source.fromFile(file)
        val lines = source.getLines.toList
        val words = getWords(lines)
        countWords(file.getName, words)
    }

    private def getWords(lines: List[String]): List[String] = {
        lines
            .map(_.toLowerCase)
            .flatMap(line => 
                line
                    .replaceAll("""[\p{Punct}“”]""", "")
                    .split(" ")
            )
            .filter(word => word != "" && !_stopWords.contains(word))
    }

    private def countWords(documentId: String, words: List[String]): Unit = {
        if (!_wordCounts.contains(documentId)) {
            _wordCounts(documentId) = Map()
        }

        words.foreach(word => {
            if (_wordCounts(documentId).contains(word)) {
                _wordCounts(documentId)(word) += 1
            } else {
                _wordCounts(documentId) += (word -> 1)
            }
        })
    }

    def getMostPopularWordsInDocument(documentId: String) = {
        _wordCounts(documentId).toList.sortBy(_._2).reverse
    }

    def getMostPopularWords = {
        _wordCounts.toList.map(_._2).flatMap(_.toList).sortBy(_._2).reverse
    }

    def calculateTFIDF = {
        val mostPopularWords = getMostPopularWords
        val tfidf = Map[String, Map[String, Double]]()

        _wordCounts.foreach(document => {
            tfidf(document._1) = Map()
            mostPopularWords.foreach(word => {
                val idf = Math.log(_wordCounts.size.toDouble / _wordCounts.count(doc => doc._2.contains(word._1)).toDouble)

                if (document._2.contains(word._1)) {
                    tfidf(document._1) += (word._1 -> document._2(word._1).toDouble / document._2.map(_._2).sum.toDouble * idf)
                } else {
                    tfidf(document._1) += (word._1 -> 0.0)
                }
            })
        })

        tfidf
    }
}

object TFIDF {
    def main(args: Array[String]): Unit = {
        val files = args.map(arg => new File(arg)).flatMap(file => {
            if (file.isDirectory) {
                file.listFiles.toList
            } else {
                List(file)
            }
        })

        val tfidf = new TFIDF()

        files.foreach(file => tfidf.parseFile(file))

        val tfidfMap = tfidf.calculateTFIDF
        
        println("Highest TFIDFs:")
        tfidfMap.foreach(doc => {
            println(doc._1)
            doc._2.toList.sortBy(_._2).reverse.take(10).foreach(word => println(s"\t${word._1} -> ${word._2}"))
        })

        println("Most popular words:")
        tfidf.getMostPopularWords.take(10).foreach(word => println(s"\t${word._1} -> ${word._2}"))

        println("Most popular words in each document:")
        files.foreach(file => {
            println(file.getName)
            tfidf.getMostPopularWordsInDocument(file.getName).take(10).foreach(word => println(s"\t${word._1} -> ${word._2}"))
        })    
    }
}
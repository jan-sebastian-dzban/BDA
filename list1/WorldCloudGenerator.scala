import java.io._
import scala.io.Source
import scala.collection.mutable.Map
import scala.io.StdIn.readLine
import scala.util.{Try,Success,Failure}

class WorldCloudGenerator {
    private val _stopWords: List[String] = Source.fromFile("stop_words_english.txt").getLines.toList
    private val _wordCounts: Map[String, Int] = Map()

    def parseFile(file: String): Unit = {
        val source = Source.fromFile(file)
        val lines = source.getLines.toList
        val words = getWords(lines)
        countWords(words)
    }

    def parseInput(input: String): Unit = {
        val lines = input.split("\n").toList
        val words = getWords(lines)
        countWords(words)
    }

    def printTopWords(n: Int): Unit = {
        println(getSortedWords.take(n).mkString("\n"))
    }

    def toCsv(file: String): Unit = {
        val writer = new PrintWriter(new File(file))
        writer.write(getSortedWords.map(word => s"${word._2}; ${word._1}").mkString("\n"))
        writer.close()
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

    private def countWords(words: List[String]): Unit = {
        words.foreach(word => {
            if (_wordCounts.contains(word)) {
                _wordCounts(word) += 1
            } else {
                _wordCounts += (word -> 1)
            }
        })
    }

    private def getSortedWords = {
        _wordCounts.toList.sortBy(_._2).reverse
    }
}

object WorldCloudGenerator {
    def main(args: Array[String]): Unit = {
        val generator = new WorldCloudGenerator
        generator.parseFile(args(0))
        generator.printTopWords(20)
        generator.toCsv("output.csv")
    }
}

object WorldCouldCLI {
    private val _parseFileCommand = raw"parse-file (.*)".r
    private val _parseInputCommand = raw"parse-input (.*)".r
    private val _printTopWordsCommand = raw"print-top-words (\d+)".r
    private val _toCsvCommand = raw"to-csv (.*)".r

    def main(args: Array[String]): Unit = {
        var generator = new WorldCloudGenerator
        var currentCommand = readLine("Enter command: ")

        while (currentCommand != "exit") {
            currentCommand match {
                case "new" => generator = new WorldCloudGenerator
                case _parseFileCommand(file) => Try(generator.parseFile(file)) match {
                    case Success(_) => println("Successfully parsed file")
                    case Failure(e) => println(s"Error parsing file: ${e.getMessage}")
                }
                case _parseInputCommand(input) => generator.parseInput(input)
                case _printTopWordsCommand(n) => generator.printTopWords(n.toInt)
                case _toCsvCommand(file) => generator.toCsv(file)
                case "help" => println("""
                    |new - create new generator
                    |parse-file <file> - parse file
                    |parse-input <input> - parse input
                    |print-top-words <n> - print top n words
                    |to-csv <file> - write to csv file
                    |exit - exit""".stripMargin
                )
                case _ => println("Unknown command")
            }
            currentCommand = readLine("Enter command: ")
        }
    }
}
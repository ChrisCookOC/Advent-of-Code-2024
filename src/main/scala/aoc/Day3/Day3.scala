package aoc.Day3

import scala.io.Source

case class Day3() {
  def findUncorruptedMuls(commands: String): Int = {

    val regex = "mul\\((\\d+),(\\d+)\\)".r

    regex.findAllMatchIn(commands)
      .map(matcher => matcher.group(1).toInt * matcher.group(2).toInt)
      .sum

  }


  def run(): Unit = {

    val file = Source.fromResource("Day3Input.txt")

    val commands = file.getLines().mkString

    val result = findUncorruptedMuls(commands)

    println(s"Result is $result")

//    val similarityScore = calculateSimilarityScore(lists._1, lists._2)
//
//    println(s"Similarity Score is $similarityScore")
  }


}

package aoc.Day3

import scala.io.Source

case class Day3() {
  def findWithConditionals(commands: String): Int = {

    val regex = "(mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\))".r

    // Don't like this but does the job
    // Should probably do some clever regex thing to only find mul that aren't after don't
    var payAttention = true

    regex
      .findAllMatchIn(commands)
      .map(matcher => {
        matcher.matched match {
          case "do()" =>
            payAttention = true
            0

          case "don't()" =>
            payAttention = false
            0
          case _ =>
            if (payAttention) matcher.group(2).toInt * matcher.group(3).toInt
            else 0

        }
      }).sum

  }

  def findUncorruptedMuls(commands: String): Int = {

    val regex = "mul\\((\\d+),(\\d+)\\)".r

    regex
      .findAllMatchIn(commands)
      .map(matcher => matcher.group(1).toInt * matcher.group(2).toInt)
      .sum

  }

  def run(): Unit = {

    val file = Source.fromResource("Day3Input.txt")

    val commands = file.getLines().mkString

    val result = findUncorruptedMuls(commands)

    println(s"Result is $result")

    val result2 = findWithConditionals(commands)

    println(s"Result is $result2")
  }

}

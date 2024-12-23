package aoc.Day4

import scala.io.Source

case class Day4() {
  def countXmas(input: String): Int = {

    val lines = input
      .split("\n")

    val map = lines.zipWithIndex
      .flatMap(line =>
        line._1
          .split("")
          .zipWithIndex
          .map(x => (x._2, line._2) -> x._1)
      )
      .toMap

    val xmas = "XMAS"
    val xmasRev = xmas.reverse

    val horizontal = lines
      .flatMap(line =>
        line
          .sliding(4)
          .map(x => x.contains(xmas) || x.contains(xmasRev))
      )
      .count(_.equals(true))

    val listTransposed = lines.map(_.split("")).transpose.map(_.mkString)

    val vertical = listTransposed
      .flatMap(line =>
        line
          .sliding(4)
          .map(x => x.contains(xmas) || x.contains(xmasRev))
      )
      .count(_.equals(true))

    val diagonalsPositive = map.groupBy { case ((x, y), _) => x + y }
    val diagonalsNegative = map.groupBy { case ((x, y), _) => x - y }

    val diagonalPositiveStrings =
      diagonalsPositive.map(items =>
        items._2.toList
          .sortBy { case ((_, y), _) => y }
          .map(_._2)
          .mkString
      )

    val diagonalNegativeStrings =
      diagonalsNegative.map(items =>
        items._2.toList
          .sortBy { case ((_, y), _) => y }
          .map(_._2)
          .mkString
      )

    val diagonalPositiveCount =
      diagonalPositiveStrings
        .flatMap(line =>
          line
            .sliding(4)
            .map(x => x.contains(xmas) || x.contains(xmasRev))
        )
        .count(_.equals(true))

    val diagonalNegativeCount =
      diagonalNegativeStrings
        .flatMap(line =>
          line
            .sliding(4)
            .map(x => x.contains(xmas) || x.contains(xmasRev))
        )
        .count(_.equals(true))

    horizontal +
      vertical +
      diagonalPositiveCount +
      diagonalNegativeCount

  }

  def run(): Unit = {

    val file = Source.fromResource("Day4Input.txt")

    val input = file.getLines().mkString("\n")

    val result = countXmas(input)

    println(s"Result is $result")

//    val result2 = findWithConditionals(commands)
//
//    println(s"Result is $result2")
  }

}

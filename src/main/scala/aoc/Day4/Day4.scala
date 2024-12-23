package aoc.Day4

import scala.io.Source

case class Day4() {
  def countCrossMas(input: String): Int = {
    val lines = input
      .split("\n")

    val sizeX = lines.head.length
    val sizeY = lines.size

    val map = lines.zipWithIndex
      .flatMap(line =>
        line._1
          .split("")
          .zipWithIndex
          .map(x => (x._2, line._2) -> x._1)
      )
      .toMap

    //Lookin' for As so pointless if they are at the edge
    (for {
      i <- 1 until sizeX -1
      j <- 1 until sizeY - 1
      if map(i, j) == "A"
      if isLeftToRight(map, i, j)
      if isRightToLeft(map, i, j)
    } yield 1).size

  }

  private def isLeftToRight(
      map: Map[(Int, Int), String],
      i: Int,
      j: Int
  ): Boolean =
    (map(i - 1, j - 1) == "M" && map(i + 1, j + 1) == "S") || (map(
      i - 1,
      j - 1
    ) == "S" && map(i + 1, j + 1) == "M")

  private def isRightToLeft(
      map: Map[(Int, Int), String],
      i: Int,
      j: Int
  ): Boolean =
    (map(i + 1, j - 1) == "M" && map(i - 1, j + 1) == "S") || (map(
      i + 1,
      j - 1
    ) == "S" && map(i - 1, j + 1) == "M")

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

    val result2 = countCrossMas(input)

    println(s"Result is $result2")
  }

}

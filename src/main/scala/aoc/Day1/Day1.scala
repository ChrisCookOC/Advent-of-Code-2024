package aoc.Day1

import scala.io.Source

case class Day1() {
  def calculateSimilarityScore(leftList: List[Int], rightList: List[Int]): Int = {

    val sortedRightList = rightList.sorted

    leftList.map(value => sortedRightList.count(x => x==value) * value)
      .sum

  }

  def getDistance(leftList: List[Int], rightList: List[Int]): Int = {

    val sortedLeftList = leftList.sorted
    val sortedRightList = rightList.sorted

    sortedLeftList.zip(sortedRightList)
      .map(value => Math.abs(value._1-value._2))
      .sum

  }


  def run(): Unit = {

    val file = Source.fromResource("Day1Input.txt")

    val lists = file.getLines()
      .map(_.split("\\s+"))
      .collect{ case Array(left, right) => (left.toInt, right.toInt)}
      .toList
      .unzip

    val result = getDistance(lists._1, lists._2)

    println(s"Distance is $result")

    val similarityScore = calculateSimilarityScore(lists._1, lists._2)

    println(s"Similarity Score is $similarityScore")
  }

}

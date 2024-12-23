package aoc.Day2

import scala.io.Source

case class Day2() {
  def howManyReportsAreSafeWithProblemDampener(reports: List[List[Int]]): Int =
    reports.count(isReportSafeWithDampener)

  def isReportSafeWithDampener(levels: List[Int]): Boolean =
    isReportSafe(levels) ||
      getCombinations(levels)
        .exists(isReportSafe)

  // Ideally you'd just remove the problem element but effort
  private def getCombinations(elements: List[Int]): List[List[Int]] = {
    elements.indices
      .map(index => {
        elements.take(index) ++ elements.drop(index + 1)
      })
      .toList
  }

  def howManyReportsAreSafe(reports: List[List[Int]]): Int =
    reports.count(isReportSafe)

  def isReportSafe(levels: List[Int]): Boolean = {

    val sortedList = levels.sorted

    if (levels == sortedList || levels.reverse == sortedList) {
      val differences = levels.sliding(2).map(x => x.max - x.min).toList
      differences.max < 4 && differences.min > 0
    } else {
      false
    }

  }

  def run(): Unit = {

    val file = Source.fromResource("Day2Input.txt")

    val lists = file.getLines().map(_.split("\\s+").map(_.toInt).toList).toList

//    val result = howManyReportsAreSafe(lists)

//    println(s"Safe report count is $result")

    val resultPD = howManyReportsAreSafeWithProblemDampener(lists)

    println(s"Safe report count with Problem Dampener is $resultPD")

  }

}

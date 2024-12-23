package aoc.Day2

import scala.io.Source

case class Day2() {
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

    val result = howManyReportsAreSafe(lists)

    println(s"Safe reports are is $result")
  }

}

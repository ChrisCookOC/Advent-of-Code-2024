package aoc.Day2

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day2Spec extends AnyWordSpec with Matchers {

  private val day2 = Day2()

  "isReportSafe" should {

    "be true when all decreasing by 1 or 2" in {
      day2.isReportSafe(List(7, 6, 4, 2, 1)) mustBe true
    }

    "be false when decrease of 5" in {
      day2.isReportSafe(List(1, 2, 7, 8, 9)) mustBe false
    }

    "be false when decrease of 4" in {
      day2.isReportSafe(List(9, 7, 6, 2, 1)) mustBe false
    }

    "be false when increasing and decreasing" in {
      day2.isReportSafe(List(1, 3, 2, 4, 5)) mustBe false
    }

    "be false when stay steady" in {
      day2.isReportSafe(List(8, 6, 4, 4, 1)) mustBe false
    }

    "be true when all increasing by 1 or 2 or 3" in {
      day2.isReportSafe(List(1, 3, 6, 7, 9)) mustBe true
    }

  }

  "howManyReportsAreSafe" should {

    "count how many are safe" in {
      val input = List(
        List(7, 6, 4, 2, 1),
        List(1, 2, 7, 8, 9),
        List(9, 7, 6, 2, 1),
        List(1, 3, 2, 4, 5),
        List(8, 6, 4, 4, 1),
        List(1, 3, 6, 7, 9)
      )
      day2.howManyReportsAreSafe(input) mustBe 2
    }

  }

}

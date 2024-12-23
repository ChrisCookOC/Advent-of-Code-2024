package aoc.Day1

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day1Spec extends AnyWordSpec with Matchers {

  private val day1 = Day1();

  "getDistance" should {

    "calculate the result" in {

      val leftList = List(3, 4, 2, 1, 3, 3)
      val rightList = List(4, 3, 5, 3, 9, 3)

     day1.getDistance(leftList, rightList) mustBe 11

    }

  }


}

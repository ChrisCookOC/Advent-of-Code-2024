package aoc.Day4

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day4Spec extends AnyWordSpec with Matchers {

  private val day4 = Day4()

  "countXmas" should {

    "count how many times it says XMAS" in {

      val input = "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"

      day4.countXmas(input) mustBe 18

    }

    "count how many times in one line" in {

      val input = "XMASXMAS"

      day4.countXmas(input) mustBe 2

    }

    "count how many times in one vertical line" in {

      val input = "X\nM\nA\nS\nX\nM\nA\nS"

      day4.countXmas(input) mustBe 2

    }

    "count how many times in one diagonal line" in {

      val input = "XAAAAAAA\nAMAAAAAA\nAAAAAAAA\nAAASAAAA\nAAAAXAAA\nAAAAAMAA\nAAAAAAAA\nAAAAAAAS"

      day4.countXmas(input) mustBe 2

    }

  }

  "countCrossMas" should {

    "count how many times it says MAS in a cross" in {

      val input = "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"

      day4.countCrossMas(input) mustBe 9

    }


  }

}

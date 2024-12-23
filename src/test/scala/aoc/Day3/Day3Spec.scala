package aoc.Day3

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day3Spec extends AnyWordSpec with Matchers {

  private val day3 = Day3()

  "findUncorruptedMuls" should {

    "only pay attention to valid multiplication commands" in {

      val sample = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

      day3.findUncorruptedMuls(sample) mustBe 161


    }

  }

}

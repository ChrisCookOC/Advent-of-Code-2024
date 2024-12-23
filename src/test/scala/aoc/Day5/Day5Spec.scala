package aoc.Day5

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day5Spec extends AnyWordSpec with Matchers {

  private val day5: Day5 = Day5()

  private val instructions = List(
    Instruction(47, 53),
    Instruction(97, 13),
    Instruction(97, 61),
    Instruction(97, 47),
    Instruction(75, 29),
    Instruction(61, 13),
    Instruction(75, 53),
    Instruction(29, 13),
    Instruction(97, 29),
    Instruction(53, 29),
    Instruction(61, 53),
    Instruction(97, 53),
    Instruction(61, 29),
    Instruction(47, 13),
    Instruction(75, 47),
    Instruction(97, 75),
    Instruction(47, 61),
    Instruction(75, 61),
    Instruction(47, 29),
    Instruction(75, 13),
    Instruction(53, 13)
  )
  private val input =
    "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47"

  "parseInput" should {

    "read the inputs and return values" in {

      val result = day5.parseInput(input)

      result._1 mustBe instructions
      result._2 mustBe List(
        List(75, 47, 61, 53, 29),
        List(97, 61, 53, 29, 13),
        List(75, 29, 13),
        List(75, 97, 47, 61, 53),
        List(61, 13, 29),
        List(97, 13, 75, 29, 47)
      )

    }

  }

  "checkRowIsRight" should {

    "really simple example" should {
      "be happy when all rules are applied" in {

        val instructions = List(Instruction(3, 43))
        val list = List(3, 43)

        day5.checkRowIsRight(list, instructions) mustBe true

      }

      "be unhappy if rule isn't applied" in {

        val instructions = List(Instruction(3, 43))
        val list = List(43, 3)

        day5.checkRowIsRight(list, instructions) mustBe false

      }

    }

    "still quite simple example" should {
      "be happy when all rules are applied" in {

        val instructions =
          List(Instruction(3, 43), Instruction(3, 67), Instruction(43, 67))
        val list = List(3, 43, 67)

        day5.checkRowIsRight(list, instructions) mustBe true

      }

      "be unhappy if rule isn't applied" in {

        val instructions =
          List(Instruction(3, 43), Instruction(3, 67), Instruction(43, 67))
        val list = List(3, 67, 43)

        day5.checkRowIsRight(list, instructions) mustBe false

      }

    }

    "supplied examples" should {
      "row 1" in {

        val list = List(75, 47, 61, 53, 29)

        day5.checkRowIsRight(list, instructions) mustBe true

      }
      "row 2" in {

        val list = List(97, 61, 53, 29, 13)

        day5.checkRowIsRight(list, instructions) mustBe true

      }

      "row 3" in {

        val list = List(75, 29, 13)

        day5.checkRowIsRight(list, instructions) mustBe true

      }

      "row 4" in {

        val list = List(75, 97, 47, 61, 53)

        day5.checkRowIsRight(list, instructions) mustBe false

      }

      "row 5" in {

        val list = List(61, 13, 29)

        day5.checkRowIsRight(list, instructions) mustBe false

      }

      "row 6" in {

        val list = List(97, 13, 75, 29, 47)

        day5.checkRowIsRight(list, instructions) mustBe false

      }

    }

  }

  "findMiddlePageNo" should {
    "find middle page number" in {

      val list = List(75, 47, 61, 53, 29)

      day5.findMiddlePageNo(list) mustBe 61

    }
  }

  "sumMiddlePagesOfValidLists" should {

    "find sum" in {
      day5.sumMiddlePagesOfValidLists(input) mustBe 143
    }
  }


}

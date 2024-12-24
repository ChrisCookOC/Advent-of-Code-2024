package aoc.Day6

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day6Spec extends AnyWordSpec with Matchers {

  private val day6 = Day6()

  "calculateHowManyVisitedFromGrid" should {
    "do everything" in {

      val grid =
        "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."

      day6.calculateHowManyVisitedFromGrid(grid) mustBe 41

    }
  }

  "howManyVisited" should {

    "count how many visited" in {
      val grid = Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> Obstacle,
        (2, 0) -> GridEmpty,
        (3, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GuardVisited,
        (2, 1) -> GuardVisited,
        (3, 1) -> Obstacle,
        (0, 2) -> GuardVisited,
        (1, 2) -> GuardVisited,
        (2, 2) -> GuardVisited,
        (3, 2) -> GridEmpty,
        (0, 3) -> GridEmpty,
        (1, 3) -> GridEmpty,
        (2, 3) -> Obstacle,
        (3, 3) -> GridEmpty
      )

      day6.howManyVisited(grid) mustBe 5

    }

  }

  "doAllTheMoves" should {
    "do all the moves" in {

      val startGrid = Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> Obstacle,
        (2, 0) -> GridEmpty,
        (3, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GuardUp,
        (2, 1) -> GridEmpty,
        (3, 1) -> Obstacle,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty,
        (3, 2) -> GridEmpty,
        (0, 3) -> GridEmpty,
        (1, 3) -> GridEmpty,
        (2, 3) -> Obstacle,
        (3, 3) -> GridEmpty
      )

      val expectedResult = Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> Obstacle,
        (2, 0) -> GridEmpty,
        (3, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GuardVisited,
        (2, 1) -> GuardVisited,
        (3, 1) -> Obstacle,
        (0, 2) -> GuardVisited,
        (1, 2) -> GuardVisited,
        (2, 2) -> GuardVisited,
        (3, 2) -> GridEmpty,
        (0, 3) -> GridEmpty,
        (1, 3) -> GridEmpty,
        (2, 3) -> Obstacle,
        (3, 3) -> GridEmpty
      )

      day6.doAllTheMoves(startGrid) mustBe expectedResult

    }
  }

  "moveGuard" should {

    "move guard up one when empty space" in {

      val grid = Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GridEmpty,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GuardUp,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )
      day6.moveGuard_int(grid, (1, 1), GuardUp) mustBe Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GuardUp,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GuardVisited,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )

    }

    "move guard down one when empty space" in {

      val grid = Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GridEmpty,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GuardDown,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )
      day6.moveGuard_int(grid, (1, 1), GuardDown) mustBe Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GridEmpty,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GuardVisited,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> GuardDown,
        (2, 2) -> GridEmpty
      )

    }

    "move guard right one when empty space" in {

      val grid = Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GridEmpty,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GuardRight,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )
      day6.moveGuard_int(grid, (1, 1), GuardRight) mustBe Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GridEmpty,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GuardVisited,
        (2, 1) -> GuardRight,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )

    }

    "move guard left one when empty space" in {

      val grid = Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GridEmpty,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GuardLeft,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )
      day6.moveGuard_int(grid, (1, 1), GuardLeft) mustBe Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GridEmpty,
        (2, 0) -> GridEmpty,
        (0, 1) -> GuardLeft,
        (1, 1) -> GuardVisited,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )

    }

    "turn guard right when obstacle is up" in {

      val grid = Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> Obstacle,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GuardUp,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )
      day6.moveGuard_int(grid, (1, 1), GuardUp) mustBe Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> Obstacle,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GuardVisited,
        (2, 1) -> GuardRight,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )

    }

    "turn guard right when obstacle is right" in {

      val grid = Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GridEmpty,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GuardRight,
        (2, 1) -> Obstacle,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )
      day6.moveGuard_int(grid, (1, 1), GuardRight) mustBe Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GridEmpty,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GuardVisited,
        (2, 1) -> Obstacle,
        (0, 2) -> GridEmpty,
        (1, 2) -> GuardDown,
        (2, 2) -> GridEmpty
      )

    }

    "turn guard right when obstacle is down" in {

      val grid = Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GridEmpty,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GuardDown,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> Obstacle,
        (2, 2) -> GridEmpty
      )
      day6.moveGuard_int(grid, (1, 1), GuardDown) mustBe Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GridEmpty,
        (2, 0) -> GridEmpty,
        (0, 1) -> GuardLeft,
        (1, 1) -> GuardVisited,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> Obstacle,
        (2, 2) -> GridEmpty
      )

    }

    "turn guard right when obstacle is left" in {

      val grid = Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GridEmpty,
        (2, 0) -> GridEmpty,
        (0, 1) -> Obstacle,
        (1, 1) -> GuardLeft,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )
      day6.moveGuard_int(grid, (1, 1), GuardLeft) mustBe Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GuardUp,
        (2, 0) -> GridEmpty,
        (0, 1) -> Obstacle,
        (1, 1) -> GuardVisited,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )

    }

    "move guard off grid up" in {

      val grid = Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GuardUp,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GridEmpty,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )
      day6.moveGuard_int(grid, (1, 0), GuardUp) mustBe Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GuardVisited,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GridEmpty,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )

    }

    "move guard off grid down" in {

      val grid = Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GridEmpty,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GridEmpty,
        (2, 1) -> GridEmpty,
        (0, 2) -> GuardDown,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )
      day6.moveGuard_int(grid, (0, 2), GuardDown) mustBe Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GridEmpty,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GridEmpty,
        (2, 1) -> GridEmpty,
        (0, 2) -> GuardVisited,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )

    }
    "move guard off grid left" in {

      val grid = Map(
        (0, 0) -> GuardLeft,
        (1, 0) -> GridEmpty,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GridEmpty,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )
      day6.moveGuard_int(grid, (0, 0), GuardLeft) mustBe Map(
        (0, 0) -> GuardVisited,
        (1, 0) -> GridEmpty,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GridEmpty,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )

    }
    "move guard off grid right" in {

      val grid = Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GridEmpty,
        (2, 0) -> GuardRight,
        (0, 1) -> GridEmpty,
        (1, 1) -> GridEmpty,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )
      day6.moveGuard_int(grid, (2, 0), GuardRight) mustBe Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> GridEmpty,
        (2, 0) -> GuardVisited,
        (0, 1) -> GridEmpty,
        (1, 1) -> GridEmpty,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )

    }

  }

  "parseGrid" should {

    "parse a grid into a map" in {
      val grid = ".#.\n.^.\n..."

      day6.parseGrid(grid) mustBe Map(
        (0, 0) -> GridEmpty,
        (1, 0) -> Obstacle,
        (2, 0) -> GridEmpty,
        (0, 1) -> GridEmpty,
        (1, 1) -> GuardUp,
        (2, 1) -> GridEmpty,
        (0, 2) -> GridEmpty,
        (1, 2) -> GridEmpty,
        (2, 2) -> GridEmpty
      )
    }

  }

}

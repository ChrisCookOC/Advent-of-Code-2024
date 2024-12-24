package aoc.Day6

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day6Spec extends AnyWordSpec with Matchers {

  private val day6 = Day6()

  "replaceCellWithObstacle" should {

    "replace cell with obstacle" in {
      val grid = Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GuardUp),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) -> GridSlot(GridEmpty)
      )

      day6.replaceCellWithObstacle(grid, (1,2)) mustBe
        Map(
          (0, 0) -> GridSlot(GridEmpty),
          (1, 0) -> GridSlot(GridEmpty),
          (2, 0) -> GridSlot(GridEmpty),
          (0, 1) -> GridSlot(GridEmpty),
          (1, 1) -> GridSlot(GuardUp),
          (2, 1) -> GridSlot(GridEmpty),
          (0, 2) -> GridSlot(GridEmpty),
          (1, 2) -> GridSlot(Obstacle),
          (2, 2) -> GridSlot(GridEmpty)
        )
    }

  }

  "calculateHowManyPlacesCouldHaveObstacles" should {

    "calculate it" in {
      val grid =
        "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."

      day6.calculateHowManyPlacesCouldHaveObstacles(grid) mustBe 6
    }

  }

  "getAddressOfAllVisitedCells" should {

    "return addresses of all visited cells" in {
      val grid = Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(Obstacle),
        (2, 0) -> GridSlot(GridEmpty),
        (3, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GridEmpty, 1),
        (2, 1) -> GridSlot(GridEmpty, 1),
        (3, 1) -> GridSlot(Obstacle),
        (0, 2) -> GridSlot(GridEmpty, 1),
        (1, 2) -> GridSlot(GridEmpty, 1),
        (2, 2) -> GridSlot(GridEmpty, 1),
        (3, 2) -> GridSlot(GridEmpty),
        (0, 3) -> GridSlot(GridEmpty),
        (1, 3) -> GridSlot(GridEmpty),
        (2, 3) -> GridSlot(Obstacle),
        (3, 3) -> GridSlot(GridEmpty)
      )

      day6.getAddressOfAllVisitedCells(grid).contains((1, 1)) mustBe true
      day6.getAddressOfAllVisitedCells(grid).contains((2, 1)) mustBe true
      day6.getAddressOfAllVisitedCells(grid).contains((0, 2)) mustBe true
      day6.getAddressOfAllVisitedCells(grid).contains((1, 2)) mustBe true
      day6.getAddressOfAllVisitedCells(grid).contains((2, 2)) mustBe true

    }

  }

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
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(Obstacle),
        (2, 0) -> GridSlot(GridEmpty),
        (3, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GridEmpty, 1),
        (2, 1) -> GridSlot(GridEmpty, 1),
        (3, 1) -> GridSlot(Obstacle),
        (0, 2) -> GridSlot(GridEmpty, 1),
        (1, 2) -> GridSlot(GridEmpty, 1),
        (2, 2) -> GridSlot(GridEmpty, 1),
        (3, 2) -> GridSlot(GridEmpty),
        (0, 3) -> GridSlot(GridEmpty),
        (1, 3) -> GridSlot(GridEmpty),
        (2, 3) -> GridSlot(Obstacle),
        (3, 3) -> GridSlot(GridEmpty)
      )

      day6.howManyVisited(grid) mustBe 5

    }

  }

  "doAllTheMoves" should {
    "do all the moves" in {

      val startGrid = Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(Obstacle),
        (2, 0) -> GridSlot(GridEmpty),
        (3, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GuardUp),
        (2, 1) -> GridSlot(GridEmpty),
        (3, 1) -> GridSlot(Obstacle),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) ->GridSlot( GridEmpty),
        (2, 2) -> GridSlot(GridEmpty),
        (3, 2) -> GridSlot(GridEmpty),
        (0, 3) -> GridSlot(GridEmpty),
        (1, 3) ->GridSlot( GridEmpty),
        (2, 3) -> GridSlot(Obstacle),
        (3, 3) -> GridSlot(GridEmpty)
      )

      val expectedResult = Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(Obstacle),
        (2, 0) -> GridSlot(GridEmpty),
        (3, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GridEmpty, 1),
        (2, 1) -> GridSlot(GridEmpty, 1),
        (3, 1) -> GridSlot(Obstacle),
        (0, 2) -> GridSlot(GridEmpty, 1),
        (1, 2) -> GridSlot(GridEmpty, 1),
        (2, 2) -> GridSlot(GridEmpty, 1),
        (3, 2) -> GridSlot(GridEmpty),
        (0, 3) -> GridSlot(GridEmpty),
        (1, 3) -> GridSlot(GridEmpty),
        (2, 3) -> GridSlot(Obstacle),
        (3, 3) -> GridSlot(GridEmpty)
      )

      day6.doAllTheMoves(startGrid) mustBe expectedResult

    }

    "stop in a loop" in {

      val startGrid = Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(Obstacle),
        (2, 0) -> GridSlot(GridEmpty),
        (3, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GuardUp),
        (2, 1) -> GridSlot(GridEmpty),
        (3, 1) -> GridSlot(Obstacle),
        (0, 2) -> GridSlot(Obstacle),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) -> GridSlot(GridEmpty),
        (3, 2) -> GridSlot(GridEmpty),
        (0, 3) -> GridSlot(GridEmpty),
        (1, 3) -> GridSlot(GridEmpty),
        (2, 3) -> GridSlot(Obstacle),
        (3, 3) -> GridSlot(GridEmpty)
      )

      val expectedResult = Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(Obstacle),
        (2, 0) -> GridSlot(GridEmpty),
        (3, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GuardUp, 2),
        (2, 1) -> GridSlot(GridEmpty, 2),
        (3, 1) -> GridSlot(Obstacle),
        (0, 2) -> GridSlot(Obstacle),
        (1, 2) -> GridSlot(GridEmpty, 2),
        (2, 2) -> GridSlot(GridEmpty, 2),
        (3, 2) -> GridSlot(GridEmpty),
        (0, 3) -> GridSlot(GridEmpty),
        (1, 3) -> GridSlot(GridEmpty),
        (2, 3) -> GridSlot(Obstacle),
        (3, 3) -> GridSlot(GridEmpty)
      )

      day6.doAllTheMoves(startGrid) mustBe expectedResult

    }

  }

  "moveGuard" should {

    "move guard up one when empty space" in {

      val grid = Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GuardUp),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) -> GridSlot(GridEmpty)
      )
      day6.moveGuard_int(grid, (1, 1), GridSlot(GuardUp)) mustBe Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GuardUp),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GridEmpty, 1),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) -> GridSlot(GridEmpty)
      )

    }

    "move guard down one when empty space" in {

      val grid = Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GuardDown),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) -> GridSlot(GridEmpty)
      )
      day6.moveGuard_int(grid, (1, 1), GridSlot(GuardDown)) mustBe Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GridEmpty, 1),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) -> GridSlot(GuardDown),
        (2, 2) -> GridSlot(GridEmpty)
      )

    }

    "move guard right one when empty space" in {

      val grid = Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GuardRight),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) -> GridSlot(GridEmpty)
      )
      day6.moveGuard_int(grid, (1, 1), GridSlot(GuardRight)) mustBe Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GridEmpty, 1),
        (2, 1) -> GridSlot(GuardRight),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) -> GridSlot(GridEmpty)
      )

    }

    "move guard left one when empty space" in {

      val grid = Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GuardLeft),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) -> GridSlot(GridEmpty)
      )
      day6.moveGuard_int(grid, (1, 1), GridSlot(GuardLeft)) mustBe Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GuardLeft),
        (1, 1) -> GridSlot(GridEmpty, 1),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) -> GridSlot(GridEmpty)
      )

    }

    "turn guard right when obstacle is up" in {

      val grid = Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(Obstacle),
        (2, 0) ->GridSlot( GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) ->GridSlot( GuardUp),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) ->GridSlot( GridEmpty),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) ->GridSlot( GridEmpty)
      )
      day6.moveGuard_int(grid, (1, 1),GridSlot( GuardUp)) mustBe Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(Obstacle),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) ->GridSlot( GridEmpty),
        (1, 1) ->GridSlot( GridEmpty, 1),
        (2, 1) -> GridSlot(GuardRight),
        (0, 2) ->GridSlot( GridEmpty),
        (1, 2) ->GridSlot( GridEmpty),
        (2, 2) -> GridSlot(GridEmpty)
      )

    }

    "turn guard right when obstacle is right" in {

      val grid = Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) ->GridSlot( GuardRight),
        (2, 1) -> GridSlot(Obstacle),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) ->GridSlot( GridEmpty),
        (2, 2) ->GridSlot( GridEmpty)
      )
      day6.moveGuard_int(grid, (1, 1), GridSlot(GuardRight)) mustBe Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GridEmpty, 1),
        (2, 1) -> GridSlot(Obstacle),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) -> GridSlot(GuardDown),
        (2, 2) -> GridSlot(GridEmpty)
      )

    }

    "turn guard right when obstacle is down" in {

      val grid = Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GuardDown),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) -> GridSlot(Obstacle),
        (2, 2) -> GridSlot(GridEmpty)
      )
      day6.moveGuard_int(grid, (1, 1), GridSlot(GuardDown)) mustBe Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GuardLeft),
        (1, 1) -> GridSlot(GridEmpty, 1),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) ->GridSlot( Obstacle),
        (2, 2) -> GridSlot(GridEmpty)
      )

    }

    "turn guard right when obstacle is left" in {

      val grid = Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(Obstacle),
        (1, 1) -> GridSlot(GuardLeft),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) ->GridSlot( GridEmpty)
      )
      day6.moveGuard_int(grid, (1, 1), GridSlot(GuardLeft)) mustBe Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GuardUp),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(Obstacle),
        (1, 1) -> GridSlot(GridEmpty, 1),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) -> GridSlot(GridEmpty)
      )

    }

    "move guard off grid up" in {

      val grid = Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GuardUp),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GridEmpty),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) -> GridSlot(GridEmpty)
      )
      day6.moveGuard_int(grid, (1, 0), GridSlot(GuardUp)) mustBe Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GridEmpty, 1),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GridEmpty),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) -> GridSlot(GridEmpty)
      )

    }

    "move guard off grid down" in {

      val grid = Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GridEmpty),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GuardDown),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) -> GridSlot(GridEmpty)
      )
      day6.moveGuard_int(grid, (0, 2), GridSlot(GuardDown)) mustBe Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GridEmpty),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GridEmpty, 1),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) ->GridSlot( GridEmpty)
      )

    }
    "move guard off grid left" in {

      val grid = Map(
        (0, 0) -> GridSlot(GuardLeft),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) ->GridSlot( GridEmpty),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) ->GridSlot( GridEmpty),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) ->GridSlot( GridEmpty)
      )
      day6.moveGuard_int(grid, (0, 0), GridSlot(GuardLeft)) mustBe Map(
        (0, 0) ->GridSlot( GridEmpty, 1),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GridEmpty),
        (2, 1) ->GridSlot( GridEmpty),
        (0, 2) ->GridSlot( GridEmpty),
        (1, 2) ->GridSlot( GridEmpty),
        (2, 2) ->GridSlot( GridEmpty)
      )

    }
    "move guard off grid right" in {

      val grid = Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GuardRight),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GridEmpty),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) -> GridSlot(GridEmpty)
      )
      day6.moveGuard_int(grid, (2, 0), GridSlot(GuardRight)) mustBe Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(GridEmpty),
        (2, 0) -> GridSlot(GridEmpty, 1),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GridEmpty),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) -> GridSlot(GridEmpty)
      )

    }

  }

  "parseGrid" should {

    "parse a grid into a map" in {
      val grid = ".#.\n.^.\n..."

      day6.parseGrid(grid) mustBe Map(
        (0, 0) -> GridSlot(GridEmpty),
        (1, 0) -> GridSlot(Obstacle),
        (2, 0) -> GridSlot(GridEmpty),
        (0, 1) -> GridSlot(GridEmpty),
        (1, 1) -> GridSlot(GuardUp),
        (2, 1) -> GridSlot(GridEmpty),
        (0, 2) -> GridSlot(GridEmpty),
        (1, 2) -> GridSlot(GridEmpty),
        (2, 2) -> GridSlot(GridEmpty)
      )
    }

  }

}

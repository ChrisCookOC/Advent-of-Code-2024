package aoc.Day6

import scala.annotation.tailrec
import scala.io.Source

case class Day6() {
  def replaceCellWithObstacle(
      grid: Map[(Int, Int), GridSlot],
      cell: (Int, Int)
  ): Map[(Int, Int), GridSlot] =
    grid.updated(cell, grid(cell).copy(cell = Obstacle, visits = 0))

  def getAddressOfAllVisitedCells(
      grid: Map[(Int, Int), GridSlot]
  ): List[(Int, Int)] =
    grid.filter(_._2.visits > 0).keys.toList

  def calculateHowManyPlacesCouldHaveObstacles(grid: String): Int = {
    val gridAsMap = parseGrid(grid)
    val finalGrid = doAllTheMoves(gridAsMap)

    val visitedCells = getAddressOfAllVisitedCells(finalGrid)
    var count = 0
    val value = visitedCells.map(cell => {
      count+=1
      println(s"$count: Seeing what happens if obstacle in $cell")
      val withObs = replaceCellWithObstacle(gridAsMap, cell)
      val finalWithObs = doAllTheMoves(withObs)
      mapStillHasGuardSoIsInLoop(finalWithObs)
    })
    value.count(_.equals(true))

  }

  def mapStillHasGuardSoIsInLoop(grid: Map[(Int, Int), GridSlot]): Boolean = {
    grid.count(x => x._2.cell.isInstanceOf[Guard]) > 0
  }

  def calculateHowManyVisitedFromGrid(grid: String): Int = {
    val gridAsMap = parseGrid(grid)
    val finalGrid = doAllTheMoves(gridAsMap)
    howManyVisited(finalGrid)
  }

  def howManyVisited(grid: Map[(Int, Int), GridSlot]): Int =
    grid.count(_._2.visits > 0)

  def doAllTheMoves(
      startGrid: Map[(Int, Int), GridSlot]
  ): Map[(Int, Int), GridSlot] =
    moveGuardsRec(startGrid)

  @tailrec
  private def moveGuardsRec(
      grid: Map[(Int, Int), GridSlot]
  ): Map[(Int, Int), GridSlot] = {
    grid.find(x => x._2.cell.isInstanceOf[Guard]) match {
      case None => grid
      case Some(_) if looping(grid) =>
        grid
      case Some((coords, guard)) =>
        val newGrid = moveGuard_int(grid, coords, guard)
        moveGuardsRec(newGrid)
    }
  }

  private def looping(grid: Map[(Int, Int), GridSlot]): Boolean =
    (grid.count(_._2.visits > 1) > 0 &&
      grid.count(_._2.visits == 1) == 0) ||
      // TODO hack alert
      //This is not sensible and is to try and get around the idea that you
      // Can have a loop inside the path that does not contain the full path
      grid.count(_._2.visits > 4) > 3

  def moveGuard_int(
      grid: Map[(Int, Int), GridSlot],
      coords: (Int, Int),
      cell: GridSlot
  ): Map[(Int, Int), GridSlot] = {
    cell.cell match {
      case GuardUp =>
        val up = (coords._1, coords._2 - 1)
        grid.get(up) match {
          case Some(slot) if slot.cell == Obstacle =>
            grid
              .updated(
                (coords._1 + 1, coords._2),
                grid(coords._1 + 1, coords._2).copy(cell = GuardRight)
              )
              .updated(
                coords,
                grid(coords).copy(cell = GridEmpty, visits = cell.visits + 1)
              )
          case Some(_) =>
            grid
              .updated(up, grid(up).copy(cell = GuardUp))
              .updated(
                coords,
                grid(coords).copy(cell = GridEmpty, visits = cell.visits + 1)
              )
          case _ =>
            grid.updated(
              coords,
              grid(coords).copy(cell = GridEmpty, visits = cell.visits + 1)
            )
        }
      case GuardDown =>
        val down = (coords._1, coords._2 + 1)
        grid.get(down) match {
          case Some(slot) if slot.cell == Obstacle =>
            grid
              .updated(
                (coords._1 - 1, coords._2),
                grid((coords._1 - 1, coords._2)).copy(cell = GuardLeft)
              )
              .updated(
                coords,
                grid(coords).copy(cell = GridEmpty, visits = cell.visits + 1)
              )
          case Some(_) =>
            grid
              .updated(down, grid(down).copy(cell = GuardDown))
              .updated(
                coords,
                grid(coords).copy(cell = GridEmpty, visits = cell.visits + 1)
              )
          case _ =>
            grid.updated(
              coords,
              grid(coords).copy(cell = GridEmpty, visits = cell.visits + 1)
            )
        }
      case GuardLeft =>
        val left = (coords._1 - 1, coords._2)
        grid.get(left) match {
          case Some(slot) if slot.cell == Obstacle =>
            grid
              .updated(
                (coords._1, coords._2 - 1),
                grid((coords._1, coords._2 - 1)).copy(cell = GuardUp)
              )
              .updated(
                coords,
                grid(coords).copy(cell = GridEmpty, visits = cell.visits + 1)
              )
          case Some(_) =>
            grid
              .updated(left, grid(left).copy(cell = GuardLeft))
              .updated(
                coords,
                grid(coords).copy(cell = GridEmpty, visits = cell.visits + 1)
              )
          case _ =>
            grid.updated(
              coords,
              grid(coords).copy(cell = GridEmpty, visits = cell.visits + 1)
            )
        }
      case GuardRight =>
        val right = (coords._1 + 1, coords._2)
        grid.get(right) match {
          case Some(slot) if slot.cell == Obstacle =>
            grid
              .updated(
                (coords._1, coords._2 + 1),
                grid((coords._1, coords._2 + 1)).copy(cell = GuardDown)
              )
              .updated(
                coords,
                grid(coords).copy(cell = GridEmpty, visits = cell.visits + 1)
              )
          case Some(_) =>
            grid
              .updated(right, grid(right).copy(cell = GuardRight))
              .updated(
                coords,
                grid(coords).copy(cell = GridEmpty, visits = cell.visits + 1)
              )
          case _ =>
            grid.updated(
              coords,
              grid(coords).copy(cell = GridEmpty, visits = cell.visits + 1)
            )
        }
    }
  }

  def parseGrid(grid: String): Map[(Int, Int), GridSlot] =
    grid
      .split("\n")
      .zipWithIndex
      .flatMap(line =>
        line._1
          .split("")
          .zipWithIndex
          .map {
            case (".", index) => (index, line._2) -> GridSlot(GridEmpty)
            case ("^", index) => (index, line._2) -> GridSlot(GuardUp)
            case ("#", index) => (index, line._2) -> GridSlot(Obstacle)
          }
      )
      .toMap

  def run(): Unit = {

    val file = Source.fromResource("Day6Input.txt")

    val input = file.getLines().mkString("\n")

    // val result = calculateHowManyVisitedFromGrid(input)

    // println(s"Result is $result")
 //TODO doesnt work
    val result2 = calculateHowManyPlacesCouldHaveObstacles(input)

    println(s"Result is $result2")

  }

}

case class GridSlot(cell: GridDetails, visits: Int = 0)

trait GridDetails
case object GridEmpty extends GridDetails

trait Guard extends GridDetails
case object GuardUp extends Guard
case object GuardDown extends Guard
case object GuardRight extends Guard
case object GuardLeft extends Guard

case object Obstacle extends GridDetails

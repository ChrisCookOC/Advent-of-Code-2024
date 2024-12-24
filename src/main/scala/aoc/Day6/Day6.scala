package aoc.Day6

import scala.annotation.tailrec
import scala.io.Source

case class Day6() {
  def calculateHowManyVisitedFromGrid(grid: String): Int = {
    val gridAsMap = parseGrid(grid)
    val finalGrid = doAllTheMoves(gridAsMap)
    howManyVisited(finalGrid)
  }

  def howManyVisited(grid: Map[(Int, Int), GridDetails]): Int =
    grid.count(_._2.equals(GuardVisited))

  def doAllTheMoves(
      startGrid: Map[(Int, Int), GridDetails]
  ): Map[(Int, Int), GridDetails] =
    moveGuardsRec(startGrid)

  @tailrec
  private def moveGuardsRec(
      grid: Map[(Int, Int), GridDetails]
  ): Map[(Int, Int), GridDetails] = {
    grid.find(x => x._2.isInstanceOf[Guard]) match {
      case None => grid
      case Some((coords, guard)) =>
        val newGrid = moveGuard_int(grid, coords, guard)
        moveGuardsRec(newGrid)
    }
  }

  def moveGuard_int(
      grid: Map[(Int, Int), GridDetails],
      coords: (Int, Int),
      guard: GridDetails
  ): Map[(Int, Int), GridDetails] = {
    guard match {
      case GuardUp =>
        val up = (coords._1, coords._2 - 1)
        grid.get(up) match {
          case Some(Obstacle) =>
            grid
              .updated((coords._1 + 1, coords._2), GuardRight)
              .updated(coords, GuardVisited)
          case Some(_) =>
            grid
              .updated(up, GuardUp)
              .updated(coords, GuardVisited)
          case _ => grid.updated(coords, GuardVisited)
        }
      case GuardDown =>
        val down = (coords._1, coords._2 + 1)
        grid.get(down) match {
          case Some(Obstacle) =>
            grid
              .updated((coords._1 - 1, coords._2), GuardLeft)
              .updated(coords, GuardVisited)
          case Some(_) =>
            grid
              .updated(down, GuardDown)
              .updated(coords, GuardVisited)
          case _ => grid.updated(coords, GuardVisited)
        }
      case GuardLeft =>
        val left = (coords._1 - 1, coords._2)
        grid.get(left) match {
          case Some(Obstacle) =>
            grid
              .updated((coords._1, coords._2 - 1), GuardUp)
              .updated(coords, GuardVisited)
          case Some(_) =>
            grid
              .updated(left, GuardLeft)
              .updated(coords, GuardVisited)
          case _ => grid.updated(coords, GuardVisited)
        }
      case GuardRight =>
        val right = (coords._1 + 1, coords._2)
        grid.get(right) match {
          case Some(Obstacle) =>
            grid
              .updated((coords._1, coords._2 + 1), GuardDown)
              .updated(coords, GuardVisited)
          case Some(_) =>
            grid
              .updated(right, GuardRight)
              .updated(coords, GuardVisited)
          case _ => grid.updated(coords, GuardVisited)
        }
    }
  }

  def parseGrid(grid: String): Map[(Int, Int), GridDetails] =
    grid
      .split("\n")
      .zipWithIndex
      .flatMap(line =>
        line._1
          .split("")
          .zipWithIndex
          .map {
            case (".", index) => (index, line._2) -> GridEmpty
            case ("^", index) => (index, line._2) -> GuardUp
            case ("#", index) => (index, line._2) -> Obstacle
          }
      )
      .toMap

//    grid.split("\n")
//      .map(line => line.split("")
//        .map {
//          case "." => GridEmpty
//          case "^" => Guard
//          case "#" => Obstacle
//        })

  def run(): Unit = {

    val file = Source.fromResource("Day6Input.txt")

    val input = file.getLines().mkString("\n")

    val result = calculateHowManyVisitedFromGrid(input)

    println(s"Result is $result")

    // val result2 = sumMiddlePagesOfInvalidListsAfterFixing(input)

    //  println(s"Result is $result2")

  }

}

trait GridDetails
case object GridEmpty extends GridDetails

trait Guard extends GridDetails
case object GuardUp extends Guard
case object GuardDown extends Guard
case object GuardRight extends Guard
case object GuardLeft extends Guard

case object Obstacle extends GridDetails
case object GuardVisited extends GridDetails

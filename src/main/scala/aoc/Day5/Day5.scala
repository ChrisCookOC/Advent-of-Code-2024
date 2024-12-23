package aoc.Day5

import scala.io.Source

case class Day5() {
  def findMiddlePageNo(list: List[Int]): Int = list(list.size / 2)

  def checkRowIsRight(
      list: List[Int],
      instructions: List[Instruction]
  ): Boolean = {

    //Collect all ones that violate rules in a list and get size
    (for {
      number <- list
      index = list.indexOf(number)
      instruction <- instructions.filter(_.first == number)
      if list.contains(instruction.second)
      if list.indexOf(instruction.second) < index
    } yield instruction).isEmpty

  }

  def parseInput(input: String): (List[Instruction], List[List[Int]]) = {

    val halves = input.split("\n\n")

    val instructions = halves.head
    val instructionList = instructions
      .split("\n")
      .map(_.split("\\|"))
      .map(x => Instruction(x.head.toInt, x.last.toInt))
      .toList

    val pages = halves.last
    val pagesList = pages
      .split("\n")
      .map(_.split(",").map(_.toInt).toList)
      .toList

    (instructionList, pagesList)

  }

  def sumMiddlePagesOfValidLists(input: String): Int = {

    val (instructions, lists) = parseInput(input)

    lists
      .map(list =>
        if (checkRowIsRight(list, instructions)) { findMiddlePageNo(list) }
        else { 0 }
      )
      .sum

  }

  def run(): Unit = {

    val file = Source.fromResource("Day5Input.txt")

        val input = file.getLines().mkString("\n")

        val result = sumMiddlePagesOfValidLists(input)

        println(s"Result is $result")
    //
    //    val result2 = countCrossMas(input)
    //
    //    println(s"Result is $result2")

  }
}

case class Instruction(first: Int, second: Int)

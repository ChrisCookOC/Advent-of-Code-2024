
import aoc.Day1.Day1
import aoc.Day2.Day2

object App {

  def main(args: Array[String]): Unit = {

    val whichDayToRun = {
      if (args.length > 0) {
        args.head
      } else {
        0
      }
    }

    whichDayToRun match {
      case "1" => Day1().run()
      case _ => Day2().run()
//      case "3" => Day3().run()
//      case "4" => Day4().run()
//      case "5" => Day5().run()
//      case "6" => Day6().run()
//      case "7" => Day7().run()
//      case "8" => Day8().run()
//      case "9" => Day9().run()
//      case _ => Day10().run()
    }
  }

}

package day20

import helpers.Helpers

object MainForProfiling extends Helpers{
  def main(args: Array[String]): Unit = {
    import PuzzleDay20.Part1._
    println(solve(resourceContent("day20/input-given-1.txt")))
  }
}

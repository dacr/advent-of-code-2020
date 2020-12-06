package day6
// SOLVED IN 22mn

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.annotation.tailrec

object PuzzleDay6 {

  object Part1 {

    def solve(input: String): Int = {
      val groups = input.split("\n\n")
      groups.map(_.filterNot(_ == '\n').distinct.length).sum
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {
    def solve(input: String): Int =  {
      val groups = input.split("\n\n")
      val groupsPeople = groups.map(_.split("\n").toList)
      groupsPeople.map(_.reduce(_ intersect _).length).sum
    }
  }
}

// =====================================================================================

class PuzzleDay6Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay6.Part1._
    solve(resourceContent("day6/input-example-1.txt")) shouldBe 11
  }
  it should "give the right result on the input file" in {
    import PuzzleDay6.Part1._
    solve(resourceContent("day6/input-given-1.txt")) should not be 7166 // because I forgot to remove some new lines
    solve(resourceContent("day6/input-given-1.txt")) shouldBe 6683
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay6.Part2._
    solve(resourceContent("day6/input-example-1.txt")) shouldBe 6
  }
  it should "give the right result on the input file" in {
    import PuzzleDay6.Part2._
    solve(resourceContent("day6/input-given-1.txt")) shouldBe 3122
  }

}

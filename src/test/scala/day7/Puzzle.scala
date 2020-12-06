package day7

// SOLVED IN 22mn

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.annotation.tailrec

object PuzzleDay7 {

  object Part1 {

    def solve(input: String): Int = {
      ???
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {
    def solve(input: String): Int = {
      ???
    }
  }

}

// =====================================================================================

class PuzzleDay7Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay7.Part1._
    solve(resourceContent("day7/input-example-1.txt")) shouldBe 42
  }
  it should "give the right result on the input file" in {
    import PuzzleDay7.Part1._
    solve(resourceContent("day7/input-given-1.txt")) shouldBe 42
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay7.Part2._
    solve(resourceContent("day7/input-example-1.txt")) shouldBe 42
  }
  it should "give the right result on the input file" in {
    import PuzzleDay7.Part2._
    solve(resourceContent("day7/input-given-1.txt")) shouldBe 42
  }

}

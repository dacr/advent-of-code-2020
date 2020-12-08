package day9

// SOLVED IN 1h15m !!

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.annotation.tailrec

object PuzzleDay9 {

  object Part1 {

    def solve(input: String): Int = {
      42
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {
    import Part1._

    def solve2(input: String): Int = {
      42
    }
  }
}

// =====================================================================================

class PuzzleDay9Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay9.Part1._
    solve(resourceContent("day9/input-example-1.txt")) shouldBe -1
  }
  it should "give the right result on the input file" in {
    import PuzzleDay9.Part1._
    solve(resourceContent("day9/input-given-1.txt")) shouldBe -1
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay9.Part2._
    solve2(resourceContent("day9/input-example-1.txt")) shouldBe -1
  }
  it should "give the right result on the input file" in {
    import PuzzleDay9.Part2._
    solve2(resourceContent("day9/input-given-1.txt")) shouldBe -1
  }

}

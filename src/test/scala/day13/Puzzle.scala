package day13

// SOLVED xx

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.math._

import scala.annotation.tailrec

object PuzzleDay13 {

  object Part1 {

    def solve(input: Iterable[String]): Long = {
      ???
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {

    import Part1._

    def solve2(input: Iterable[String]): Long = {
      ???
    }

  }

}

// =====================================================================================

class PuzzleDay13Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay13.Part1._
    solve(resourceContentLines("day13/input-example-1.txt")) shouldBe -1
  }
  it should "give the right result on the input file" in {
    import PuzzleDay13.Part1._
    solve(resourceContentLines("day13/input-given-1.txt")) shouldBe -1
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay13.Part2._
    solve2(resourceContentLines("day13/input-example-1.txt")) shouldBe -1
  }
  it should "give the right result on the input file" in {
    import PuzzleDay13.Part2._
    solve2(resourceContentLines("day13/input-given-1.txt")) shouldBe -1
  }

}

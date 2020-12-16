package day17

// solved in ~1h15m

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.math._

import scala.annotation.tailrec

object PuzzleDay17 {

  object Part1 {

    def solve(input: String): Long = {
      42L
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {

    def solve(input: String): Long = {
      42L
    }

  }

}

// =====================================================================================

class PuzzleDay17Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay17.Part1._
    solve(resourceContent("day17/input-example-1.txt")) shouldBe -1
  }
  it should "give the right result on the input file" in {
    import PuzzleDay17.Part1._
    solve(resourceContent("day17/input-given-1.txt")) shouldBe -1
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" ignore {
    import PuzzleDay17.Part2._
    solve(resourceContent("day17/input-example-1.txt")) shouldBe -1
  }
  it should "give the right result on the input file" in {
    import PuzzleDay17.Part2._
    solve(resourceContent("day17/input-given-1.txt")) shouldBe -1
  }

}

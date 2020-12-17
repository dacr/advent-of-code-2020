package day18

// solved in 45mn !

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.math._

import scala.annotation.tailrec

object PuzzleDay18 {


  object Part1 {

    def solve(input: String): Int = ???

  // -------------------------------------------------------------------------

  object Part2 {

    def solve(input: String): Int = ???
  }

}

// =====================================================================================

class PuzzleDay18Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay18.Part1._
    solve(resourceContent("day18/input-example-1.txt")) shouldBe -1
  }
  it should "give the right result on the input file" in {
    import PuzzleDay18.Part1._
    solve(resourceContent("day18/input-given-1.txt")) shouldBe -1
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay18.Part2._
    solve(resourceContent("day18/input-example-1.txt")) shouldBe -1
  }
  it should "give the right result on the input file" in {
    import PuzzleDay18.Part2._
    solve(resourceContent("day18/input-given-1.txt")) shouldBe -1
  }

}

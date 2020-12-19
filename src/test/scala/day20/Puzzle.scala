package day20

// solved in XXmn

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.util.regex.Pattern
import scala.math._
import scala.annotation.tailrec
import scala.util.matching.Regex

object PuzzleDay20 {

  object Part1 {


    def solve(input: String): Long = {
      ???
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {

    def solve(input: String): Long = {
      ???
    }
  }

}

// =====================================================================================

class PuzzleDay20Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay20.Part1._
    solve(resourceContent("day20/input-example-1.txt")) shouldBe -1
  }
  it should "give the right result on the input file" in {
    import PuzzleDay20.Part1._
    solve(resourceContent("day20/input-given-1.txt")) shouldBe -1
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay20.Part2._
    solve(resourceContent("day20/input-example-1.txt")) shouldBe -1
  }
  it should "give the right result on the input file" in {
    import PuzzleDay20.Part2._
    solve(resourceContent("day20/input-given-1.txt")) shouldBe -1
  }
}

package day6
// SOLVED IN XXmn

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.annotation.tailrec

object PuzzleDay6 {

  object Part1 {

    def solve(input: Iterable[String]): Int = {
      ???
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {
    def solve2(input: Iterable[String]): Int =  {
      ???
    }
  }
}

// =====================================================================================

class PuzzleDay6Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay6.Part1._
    solve(resourceContentLines("day6/input-given-1.txt")) shouldBe 42
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay6.Part2._
    solve2(resourceContentLines("day6/input-given-1.txt")) shouldBe 42
  }

}

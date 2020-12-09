package day10

// SOLVED IN 1h15m !! not in good shape this morning ! because it was not too hard

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.annotation.tailrec

object PuzzleDay10 {

  object Part1 {


    def solve(input: Iterable[String]): Long = {
      42
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {
    import Part1._

    def solve2(input: Iterable[String]): Long = {
      42
    }
  }
}

// =====================================================================================

class PuzzleDay10Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay10.Part1._
    solve(resourceContentLines("day10/input-example-1.txt")) shouldBe -1
  }
  it should "give the right result on the input file" in {
    import PuzzleDay10.Part1._
    solve(resourceContentLines("day10/input-given-1.txt")) shouldBe -1
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay10.Part2._
    solve2(resourceContentLines("day10/input-example-1.txt")) shouldBe -1
  }
  it should "give the right result on the input file (functional)" in {
    import PuzzleDay10.Part2._
    solve2(resourceContentLines("day10/input-given-1.txt")) shouldBe -1
  }

}

package day5

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

object PuzzleDay5 {

  object Part1 {

    def solve(input: String): Int = ???
  }

  // -------------------------------------------------------------------------

  object Part2 {

    def solve(input: String): Int = ???
  }
}

// =====================================================================================

class PuzzleDay5Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay5.Part1._
    solve(resourceContent("day5/input-example-1.txt")) shouldBe 0
  }

  it should "give the right answer on the given file" in {
    import PuzzleDay5.Part1._
    solve(resourceContent("day5/input-given-1.txt")) shouldBe 0
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay5.Part2._
    solve(resourceContent("day5/input-example-1.txt")) shouldBe 0
  }

  it should "give the right answer on the given file" in {
    import PuzzleDay5.Part2._
    solve(resourceContent("day5/input-given-1.txt")) shouldBe 0
  }

}

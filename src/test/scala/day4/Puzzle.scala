package day4

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

object PuzzleDay4 {

  def solve1(input: Iterable[String]): Int = {
    ???
  }

  // -------------------------------------------------------------------------

  def solve2(input: Iterable[String]): Long = {
    ???
  }
}

// =====================================================================================

class PuzzleDay4Test extends AnyFlatSpec with should.Matchers with Helpers {

  import PuzzleDay4._

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    solve1(resourceContent("day4/input-example-1.txt")) shouldBe 7
  }

  it should "give the right answer on the given file" in {
    solve1(resourceContent("day4/input-given-1.txt")) shouldBe 276
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    solve2(resourceContent("day4/input-example-1.txt")) shouldBe 336
  }

  it should "give the right answer on the given file" in {
    solve2(resourceContent("day4/input-given-1.txt")) shouldBe 7812180000L
  }

}

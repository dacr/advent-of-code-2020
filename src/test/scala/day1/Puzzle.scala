package day1
// SOLVED IN ??mn but very quickly a few minutes

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

object PuzzleDay1 {
  def solve1(input:Iterable[String]):Long = {
    input
      .map(_.toInt)
      .to(List)
      .combinations(2)
      .filter(_.sum == 2020)
      .to(LazyList)
      .headOption
      .map{_.product}
      .get
  }

  def solve2(input:Iterable[String]):Long = {
    input
      .map(_.toInt)
      .to(List)
      .combinations(3)
      .filter(_.sum == 2020)
      .to(LazyList)
      .headOption
      .map{_.product}
      .get
  }

}

class PuzzleDay1Test extends AnyFlatSpec with should.Matchers with Helpers {
  import PuzzleDay1._

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    solve1(resourceContentLines("day1/input-example-1.txt")) shouldBe 514579
  }

  it should "give the right answer on the given file" in {
    solve1(resourceContentLines("day1/input-given-1.txt")) shouldBe 902451
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    solve2(resourceContentLines("day1/input-example-1.txt")) shouldBe 241861950
  }

  it should "give the right answer on the given file" in {
    solve2(resourceContentLines("day1/input-given-1.txt")) shouldBe 85555470
  }

}

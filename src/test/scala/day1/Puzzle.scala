package day1

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

object Puzzle {
  def solve1(input:Iterable[String]):Long = {
    input
      .map(_.toInt)
      .toList
      .combinations(2)
      .filter(_.sum == 2020)
      .to(LazyList)
      .headOption
      .map{_.reduce(_ * _)}
      .get
  }

  def solve2(input:Iterable[String]):Long = {
    input
      .map(_.toInt)
      .toList
      .combinations(3)
      .filter(_.sum == 2020)
      .to(LazyList)
      .headOption
      .map{_.reduce(_ * _)}
      .get
  }

}

class PuzzleTest extends AnyFlatSpec with should.Matchers with Helpers {
  import Puzzle._

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    solve1(resourceContent("day1/input-example-1.txt")) shouldBe 514579
  }

  it should "give the right answer on the given file" in {
    solve1(resourceContent("day1/input-given-1.txt")) shouldBe 902451
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    solve2(resourceContent("day1/input-example-1.txt")) shouldBe 241861950
  }

  it should "give the right answer on the given file" in {
    solve2(resourceContent("day1/input-given-1.txt")) shouldBe 85555470
  }

}

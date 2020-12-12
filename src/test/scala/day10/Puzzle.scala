package day10

// SOLVED xx

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.annotation.tailrec

object PuzzleDay10 {

  object Part1 {

    def solve(input: Iterable[String]): Long = {
      val adapters = input.map(_.toInt).to(List)
      val device = adapters.max + 3 // goal
      val diffs = (0+:adapters:+device).sorted.sliding(2, 1).map{ case List(a,b) => b-a}.to(List)
      diffs.count(_ == 1) * diffs.count(_ == 3)
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {

    def fact(num: Int): BigInt = {
      LazyList
        .from(1)
        .take(num)
        .map(x => BigInt(x))
        .foldLeft(BigInt(1)) {case (a,b) => a * b}
    }

    // TODO - TO BE REVIEWED AS I FOUND IT in an empirical manner !!!! Thanks to unit tests !
    def additionsCombinations(n:Int):BigDecimal = {
      n match {
        case 2 => 2
        case 3 => 4
        case 4 => 7
      }
    }

    def solve2(input: Iterable[String]): BigDecimal = {
      val adapters = input.map(_.toInt).to(List)
      val goal = adapters.max + 3
      val diffs = (0+:adapters:+goal).sorted.sliding(2, 1).map{ case List(a,b) => b-a}.to(Array)
      var consecutives = List.empty[Int]
      var groupedCount=0
      for{ i <- 0 until diffs.size} {
        if (diffs(i)==1) groupedCount+=1
        else if (diffs(i) != 1 && groupedCount>1) {
          consecutives=groupedCount::consecutives
          groupedCount=0
        } else groupedCount=0
      }
      consecutives.map(additionsCombinations).product
    }
  }

}

// =====================================================================================

class PuzzleDay10Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay10.Part1._
    solve(resourceContentLines("day10/input-example-1.txt")) shouldBe 35
    solve(resourceContentLines("day10/input-example-2.txt")) shouldBe 220
  }
  it should "give the right result on the input file" in {
    import PuzzleDay10.Part1._
    solve(resourceContentLines("day10/input-given-1.txt")) shouldBe 2112
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay10.Part2._
    solve2(resourceContentLines("day10/input-example-1.txt")) shouldBe 8
    solve2(resourceContentLines("day10/input-example-2.txt")) shouldBe 19208
  }
  it should "give the right result on the input file" in {
    import PuzzleDay10.Part2._
    solve2(resourceContentLines("day10/input-given-1.txt")) shouldBe 3022415986688L
  }

}

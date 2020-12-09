package day9

// SOLVED IN 1h15m !! not in good shape this morning ! because it was not too hard

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.annotation.tailrec

object PuzzleDay9 {

  object Part1 {

    def sumOf(input:Array[Long]):Boolean = {
      val goal = input.last
      val from = input.init.combinations(2).toList
      from.exists{case Array(a,b) => a+b == goal}
    }

    def solve(input: Iterable[String], window:Int=25): Long = {
      val entries = input.map(_.toLong).toArray
      val results =
        entries
          .inits
          .toList
          .reverse
          .map(_.takeRight(window+1))
          .filter(_.size>window)
          .filterNot(sumOf)
          .headOption
          .flatMap(_.lastOption)

      results.getOrElse(-1L)
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {
    import Part1._

    def solve2Functional(input: Iterable[String], invalidNumber:Long): Long = {
      input
        .map(_.toLong)
        .to(Array)
        .tails
        .flatMap(_.inits.find(_.sum == invalidNumber))
        .map(found => found.min + found.max)
        .to(LazyList)
        .headOption
        .getOrElse(-1L)
    }

    def solve2FunctionalAlt(input: Iterable[String], invalidNumber:Long): Long = {
      def lookIn(remains:Iterable[Long], acc:List[Long]=Nil, sum:Long=0L):List[Long] = {
        if (sum==invalidNumber) acc
        else if (sum>invalidNumber || remains.isEmpty) Nil
        else lookIn(remains.tail, remains.head::acc, remains.head+sum)
      }
      input
        .map(_.toLong)
        .to(List)
        .tails
        .map(nums => lookIn(nums))
        .filter(_.size>0)
        .map(found => found.min + found.max)
        .to(LazyList)
        .headOption
        .getOrElse(-1L)
    }

    def solve2BabyCode(input: Iterable[String], invalidNumber:Long): Long = {
      val entries = input.map(_.toLong).toArray
      var currentSelected = List.empty[Long]
      var currentSum = 0L
      var from = 0
      var current = 0
      while(currentSum != invalidNumber) {
        val value = entries(current)
        currentSelected = value::currentSelected
        currentSum+=value
        if (currentSum > invalidNumber) {
          currentSum=0L
          currentSelected = List.empty[Long]
          from+=1
          current=from
        } else current+=1
      }
      currentSelected.min + currentSelected.max
    }
  }
}

// =====================================================================================

class PuzzleDay9Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay9.Part1._
    solve(resourceContentLines("day9/input-example-1.txt"), 5) shouldBe 127
  }
  it should "give the right result on the input file" in {
    import PuzzleDay9.Part1._
    solve(resourceContentLines("day9/input-given-1.txt")) shouldBe 15353384
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay9.Part2._
    solve2Functional(resourceContentLines("day9/input-example-1.txt"), 127L) shouldBe 62
  }
  it should "give the right result on the input file (baby code)" in {
    import PuzzleDay9.Part2._
    solve2BabyCode(resourceContentLines("day9/input-given-1.txt"), 15353384L) shouldBe 2466556
  }
  it should "give the right result on the input file (functional)" in {
    import PuzzleDay9.Part2._
    solve2Functional(resourceContentLines("day9/input-given-1.txt"), 15353384L) shouldBe 2466556
  }
  it should "give the right result on the input file (functional alternative)" in {
    import PuzzleDay9.Part2._
    solve2FunctionalAlt(resourceContentLines("day9/input-given-1.txt"), 15353384L) shouldBe 2466556
  }

}

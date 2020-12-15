package day15

// SOLVED 1h30m but directly without mutations or variables

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.math._

import scala.annotation.tailrec

object PuzzleDay15 {

  object Part1 {

    def solve(input: String): Long = {
      val numbers=input.split(",").map(_.toInt)

      @tailrec
      def play(round:Int, previousSpoken:Int, cache:Map[Int,List[Int]]):Int = {
        if (round-1 == 2020) previousSpoken
        else {
          val spoken = {
            if (round-1 < numbers.size) numbers(round-1)
            else cache.get(previousSpoken) match {
              case Some(a::Nil) => 0
              case Some(a::b::_)=> a-b
            }
          }
          val newCache = cache.updated(spoken, round::cache.get(spoken).map(_.take(2)).getOrElse(Nil) )
          play(round+1, spoken, newCache)
        }
      }
      play(1,-1, Map.empty)
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {

    def solve(input: String, limit:Int): Long = {
      val numbers=input.split(",").map(_.toInt)
      // Of course quite faster with a mutable map
      @tailrec
      def play(round:Int, previousSpoken:Int, cache:collection.mutable.Map[Int,List[Int]]):Int = {
        if (round-1 == limit) previousSpoken
        else {
          val spoken = {
            if (round-1 < numbers.size) numbers(round-1)
            else cache.get(previousSpoken) match {
              case Some(a::Nil) => 0
              case Some(a::b::_)=> a-b
            }
          }
          cache.put(spoken, round::cache.get(spoken).map(_.take(2)).getOrElse(Nil) )
          play(round+1, spoken, cache)
        }
      }
      play(1,-1, collection.mutable.Map.empty)
    }
  }

}

// =====================================================================================

class PuzzleDay15Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay15.Part1._
    solve("0,3,6") shouldBe 436
    solve("1,3,2") shouldBe 1
    solve("2,1,3") shouldBe 10
    solve("1,2,3") shouldBe 27
    solve("2,3,1") shouldBe 78
    solve("3,2,1") shouldBe 438
    solve("3,1,2") shouldBe 1836
  }
  it should "give the right result on the input file" in {
    import PuzzleDay15.Part1._
    solve("12,20,0,6,1,17,7") shouldBe 866
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay15.Part2._
    val limit = 30000000
    solve("0,3,6", limit) shouldBe 175594
    solve("1,3,2", limit) shouldBe 2578
    solve("2,1,3", limit) shouldBe 3544142
    solve("1,2,3", limit) shouldBe 261214
    solve("2,3,1", limit) shouldBe 6895259
    solve("3,2,1", limit) shouldBe 18
    solve("3,1,2", limit) shouldBe 362
  }

  it should "give the right result on the input file" in {
    import PuzzleDay15.Part2._
    val limit = 30000000
    solve("12,20,0,6,1,17,7", limit) shouldBe 1437692
  }

}

package day13

// SOLVED xx

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.math._

import scala.annotation.tailrec

object PuzzleDay13 {

  object Part1 {

    def solve(input: Iterable[String]): Int = {
      val earliestTimestamp = input.head.toInt
      val buses = input.tail.head.split(",").filterNot(_=="x").map(_.toInt)
      LazyList
        .from(earliestTimestamp)
        .flatMap{case timestamp => buses.find(timestamp % _ == 0).map(timestamp -> _)}
        .headOption
        .map{case (t,b) => (t-earliestTimestamp)*b}
        .getOrElse(0)
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {

    def solve(input: Iterable[String], startFrom:Long): Long = {
      solve(input.tail.head, startFrom)
    }
    def solve(input:String, startFrom:Long=0L):Long = {
      val buses =
        input
          .split(",")
          .zipWithIndex
          .collect{case (busId, i) if busId!="x" => busId.toInt->i }

      val (maxBusId,maxBusIdPos) = buses.maxBy{case (busId,_)=>busId}
      val otherBuses = buses.filterNot{case (busId,_) => busId == maxBusId}
      val maxPosWithConstraint = buses.map{case (_,pos) => pos}.max

      def check(t:Long):Boolean = {
        otherBuses.forall{case (id, pos) => (t - maxBusIdPos + pos)%id == 0L}
      }
      def dump(timestamp:Long):Unit = {
        println("timestamp "+buses.map{case (id,_)=>id}.mkString(" "))
        for {
          i <- (-maxBusIdPos to ( maxPosWithConstraint-maxBusIdPos ))
          t = timestamp + i
        } println(s"$t "+buses.map{case (id,pos) => t % id == 0L}.map{case true => "D" case _ => "."}.mkString(" "))
      }

      val result =
        LazyList
          .iterate(startFrom/maxBusId*maxBusId)(_+maxBusId)
          .find(check)
          .headOption
          .map(t => t-maxBusIdPos)
          .get
      dump(result)
      result
    }

  }

}

// =====================================================================================

class PuzzleDay13Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay13.Part1._
    solve(resourceContentLines("day13/input-example-1.txt")) shouldBe 295
  }
  it should "give the right result on the input file" in {
    import PuzzleDay13.Part1._
    solve(resourceContentLines("day13/input-given-1.txt")) shouldBe 2545
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay13.Part2._
    solve(resourceContentLines("day13/input-example-1.txt"),0L) shouldBe 1068781L
  }
  it should "give the right result on the example again;)" in {
    import PuzzleDay13.Part2._
    solve("7,13,x,x,59,x,31,19") shouldBe 1068781L
  }
  it should "give the right on the other given examples" in {
    import PuzzleDay13.Part2._
    solve("17,x,13,19") shouldBe 3417L
    solve("17,x,13,19", 3000L) shouldBe 3417L
    solve("67,7,59,61") shouldBe 754018L
    solve("67,x,7,59,61") shouldBe 779210L
    solve("67,7,x,59,61") shouldBe 1261476L
    solve("1789,37,47,1889") shouldBe 1202161486L
  }
  it should "give the right result on the input file" in {
    import PuzzleDay13.Part2._
    solve(resourceContentLines("day13/input-given-1.txt"), 100_000_000_000_000L) shouldBe -1L
  }

}

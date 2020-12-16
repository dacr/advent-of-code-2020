package day13

// SOLVED quickly first - UNSOLVED THE second PART - LEFT AS IS

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.math._

import scala.annotation.tailrec

object PuzzleDay13 {

  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long = if (a == 0 || b == 0) 0 else a * b / gcd(a, b)

  def gcds(nums: Iterable[Long]): Long = nums.reduce(gcd)

  def lcms(nums: Iterable[Long]): Long = nums.reduce(lcm)

  def gcds(nums: Long*): Long = nums.reduce(gcd)

  def lcms(nums: Long*): Long = nums.reduce(lcm)


  object Part1 {

    def solve(input: Iterable[String]): Int = {
      val earliestTimestamp = input.head.toInt
      val buses = input.tail.head.split(",").filterNot(_ == "x").map(_.toInt)
      LazyList
        .from(earliestTimestamp)
        .flatMap { case timestamp => buses.find(timestamp % _ == 0).map(timestamp -> _) }
        .headOption
        .map { case (t, b) => (t - earliestTimestamp) * b }
        .getOrElse(0)
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {

    def solve(input: Iterable[String], startFrom: Long): Long = {
      solve(input.tail.head, startFrom)
    }


    def solve(input: String, startFrom:Long=0L): Long = {
      val buses =
        input
          .split(",")
          .zipWithIndex
          .collect { case (busId, i) if busId != "x" => busId.toLong -> i.toLong }

      val (maxBusId, maxBusIdPos) = buses.maxBy { case (busId, _) => busId }
      val otherBuses = buses.filterNot { case (busId, _) => busId == maxBusId }

      def check(v: Long): Boolean = {
        val t = v - maxBusIdPos
        otherBuses.forall { case (id, pos) =>
          (t + pos) % id == 0L
        }
      }

//      LazyList
//        .from(1)
//        .map(_ * maxBusId.toLong)
//        .find(check)
//        .headOption
//        .map(t => t - maxBusIdPos)
//        .get

      // 100000000000000L
      //  99999999999762L
      @tailrec
      def worker(timestamp:Long):Long = {
        if (check(timestamp)) timestamp
        else worker(timestamp+maxBusId)
      }
      worker(startFrom/maxBusId*maxBusId) - maxBusIdPos
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
    solve(resourceContentLines("day13/input-example-1.txt"), 0L) shouldBe 1068781L
  }
  it should "give the right result on a very simple example#1)" in {
    import PuzzleDay13.Part2._
    solve("3,5") shouldBe 9L
  }
  it should "give the right result on a very simple example#2)" in {
    import PuzzleDay13.Part2._
    solve("3,x,7") shouldBe 12L
  }
  it should "give the right result on a very simple example#3)" in {
    import PuzzleDay13.Part2._
    solve("3,5,7") shouldBe 54L
  }
  it should "give the right on the other given examples" in {
    import PuzzleDay13.Part2._
    solve("17,x,13,19") shouldBe 3417L
    solve("17,x,13,19",3000L) shouldBe 3417L
    solve("67,7,59,61") shouldBe 754018L
    solve("67,x,7,59,61") shouldBe 779210L
    solve("67,7,x,59,61") shouldBe 1261476L
    solve("1789,37,47,1889") shouldBe 1_202_161_486L
  }
  it should "give the right result on the input file" ignore { // ignored because brute force implementation...
    import PuzzleDay13.Part2._
    solve(resourceContentLines("day13/input-given-1.txt"), 180_000_000_000_000L) shouldBe 266204454441577L
  }

}

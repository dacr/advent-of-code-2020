package day5

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.annotation.tailrec

object PuzzleDay5 {

  object Part1 {

    def decodeRow(rows:String):Int = {
      @tailrec
      def worker(remaining:List[Char], low:Int,up:Int):Int = {
        remaining match {
          case ch::Nil if ch=='F' => math.min(low,up)
          case ch::Nil if ch=='B' => math.max(low,up)
          case ch::tail if ch=='F' => worker(tail, low, low+(up-low)/2)
          case ch::tail if ch=='B' => worker(tail, low+(up-low)/2+1, up)
        }
      }
      worker(rows.to(List), 0, 127)
    }

    def decodeCol(cols:String):Int = {
      @tailrec
      def worker(remaining:List[Char], left:Int, right:Int):Int = {
        remaining match {
          case ch::Nil if ch=='R' => math.max(left,right)
          case ch::Nil if ch=='L' => math.min(left,right)
          case ch::tail if ch=='L' => worker(tail, left, left+(right-left)/2)
          case ch::tail if ch=='R' => worker(tail, left+(right-left)/2+1, right)
        }
      }
      worker(cols.to(List), 0, 7)
    }

    def decode(input:String):(Int,Int) = {
      val rows=input.take(7)
      val cols=input.drop(7)
      val row=decodeRow(rows)
      val col=decodeCol(cols)
      (row,col)
    }
    def seatToId(pos:(Int,Int)) = {
      pos match {case (r,c) => r*8+c}
    }

    def solve(input: Iterable[String]): Int = {
      input
        .map(decode)
        .map(seatToId)
        .max
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {
    import Part1._
    def aloneSeats(input:Iterable[String]):List[Int] = {
      val allSeatsId = 0.to(127*8+7).toSet
      val occupiedSeatsId = input.map(decode).map(seatToId).toSet
      (allSeatsId -- occupiedSeatsId).toList.sorted
    }
    def solve2(input: Iterable[String]): Int =  {
      val candidates = aloneSeats(input)
      // remove all entries which have seats +1 or -1
      42
    }
  }
}

// =====================================================================================

class PuzzleDay5Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay5.Part1._
    decode("FBFBBFFRLR") shouldBe (44,5)
    decode("BFFFBBFRRR") shouldBe (70,7)
    decode("FFFBBBFRRR") shouldBe (14,7)
    decode("BBFFBBFRLL") shouldBe (102,4)
  }

  it should "give the right answer on the given file" in {
    import PuzzleDay5.Part1._
    solve(resourceContentLines("day5/input-given-1.txt")) should not be 60416
    solve(resourceContentLines("day5/input-given-1.txt")) shouldBe 864
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay5.Part2._
    solve2(resourceContentLines("day5/input-given-1.txt")) shouldBe 739
  }

}

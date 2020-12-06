package day5
// SOLVED IN 45mn

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.annotation.tailrec

object PuzzleDay5 {

  object Part1 {

    def input2pos(input:String, lowLimit:Int, lowChar:Char, upLimit:Int, upChar:Int):Int = {
      input.foldLeft( (lowLimit,upLimit) ) {
        case ( (low,up), ch) if ch == lowChar => (low, low+(up-low)/2)
        case ( (low,up), ch) if ch == upChar => (low+(up-low)/2+1, up)
      } match {
        case (low, up) if input.last == lowChar => math.min(low,up)
        case (low, up) if input.last == upChar => math.max(low,up)
      }
    }

    def decodeRow(rows:String):Int = input2pos(rows, 0, 'F', 127, 'B')

    def decodeCol(cols:String):Int = input2pos(cols, 0, 'L', 7, 'R')

    def decode(input:String):(Int,Int) = (decodeRow(input.take(7)), decodeCol(input.drop(7)))

    def seatToId(pos:(Int,Int)): Int = pos match {case (r,c) => r*8+c}

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
    def aloneSeats(input:Iterable[String]):Set[Int] = {
      val allSeatsId = 0.to(127*8+7).toSet
      val occupiedSeatsId = input.map(decode).map(seatToId).toSet
      allSeatsId -- occupiedSeatsId
    }
    def solve2(input: Iterable[String]): Int =  {
      val candidates = aloneSeats(input)
      // remove all entries which have seats +1 or -1 (got the right response thanks to a println / debug of candidates content ;) )
      candidates
        .filterNot(v => candidates.contains(v-1))
        .filterNot(v => candidates.contains(v+1))
        .headOption.getOrElse(-1)
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

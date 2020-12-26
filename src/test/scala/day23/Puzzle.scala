package day23

// solved in XXmn

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.math._
import scala.annotation.tailrec
import scala.collection.immutable.Queue

object PuzzleDay23 {

  object Part1 {

    type Cup=Int
    case class GameState(cups:Vector[Cup]) {
      def cup:Cup = cups.head
      def destCup:Cup = {
        @tailrec def check(thatCup:Int):Int = {
          if (thatCup == 0) check(cups.length) else {
            val candidatePos = cups.indexWhere(_ == thatCup)
            if (candidatePos > 3 ) thatCup
            else check(thatCup - 1)
          }
        }
        check(cup - 1)
      }

      override def toString() = cups.mkString+s" $cup->$destCup"

      def result():String = {
        cups.splitAt(cups.indexWhere(_ == 1)) match {
          case (left, right) => right.tail.mkString + left.mkString
        }
      }
    }

    def play(moves:Int, state:GameState):GameState = {
      if (moves == 0) state else {
        val pickedUp = state.destCup +: state.cups.tail.take(3)
        val (left,right) = state.cups.splitAt(state.cups.indexWhere(_ == state.destCup))
        val newCupsOrdering = left.filterNot(cup => pickedUp.contains(cup)) ++ pickedUp ++ right.tail.filterNot(cup => pickedUp.contains(cup))
        play(moves - 1, GameState(newCupsOrdering.tail:+newCupsOrdering.head))
      }
    }


    def solve(input: String): String = {
      val cups = input.trim.split("").map(_.toInt).to(Vector)
      val state = GameState(cups)
      val lastState = play(100, state)
      lastState.result()
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {
    type Cup=Int
    type Links=Map[Cup,Cup]

    def solve(input: String, moves:Int=10_000_000, maxCup:Int=1_000_000): Long = {
      val givenCups     = input.trim.split("").map(_.toInt).to(Array)
      val cups          = (givenCups++:10.to(maxCup))
      val forwardLinks  = cups.sliding(2,1).map{case IndexedSeq(from,to)=>from->to}.toMap+(cups.last->cups.head)
      val startCup      = cups.head

      @tailrec
      def play(currentCup:Cup, remainingMoves:Int, forwards:Links):Links = {
        if (remainingMoves==0) forwards else {
          val forward1 = forwards(currentCup)
          val forward2 = forwards(forwards(currentCup))
          val forward3 = forwards(forwards(forwards(currentCup)))
          @tailrec
          def findNextCup(fromCup:Cup, forwards:Links):Cup = {
            val nextCup = if (fromCup > 0) fromCup else forwards.size
            if (nextCup == forward1 ||
                nextCup == forward2 ||
                nextCup == forward3) findNextCup(nextCup - 1, forwards)
            else nextCup
          }
          val nextCup = findNextCup(currentCup - 1, forwards)
          val newForwardLinks =
            forwards
              .updated(currentCup, forwards(forward3))
              .updated(nextCup, forward1)
              .updated(forward3, forwards(nextCup))
          play(newForwardLinks(currentCup), remainingMoves-1, newForwardLinks)
        }
      }
      val lastForwardLinks = play(startCup, moves, forwardLinks)
      val f1 = lastForwardLinks(1)
      val f2 = lastForwardLinks(f1)
      f1.toLong*f2
    }

  }

}

// =====================================================================================

class PuzzleDay23Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay23.Part1._
    solve("389125467") shouldBe "67384529"
  }
  it should "give the right result on the input file" in {
    import PuzzleDay23.Part1._
    solve("389547612") shouldBe "45286397"
  }

  // ------------------------------------------------------------------------------------


  "puzzle star#2 example" should "give the right result on previous cases" in {
    import PuzzleDay23.Part2._
    solve("389125467", 100, 9) shouldBe 42
    solve("389547612", 100, 9) shouldBe 20
  }
  it should "give the right result on the example" in {
    import PuzzleDay23.Part2._
    solve("389125467") shouldBe 149245887792L
  }
  it should "give the right result on the input file" in {
    import PuzzleDay23.Part2._
    solve("389547612") should not be 3999944L
    solve("389547612") shouldBe 836763710L
  }
}

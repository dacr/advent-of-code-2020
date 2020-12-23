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
    type Cups=Array[Cup]
    type Pointer=Int
    type Pointers=Array[Pointer]

    case class GameState(current:Pointer, cups:Cups, pointers:Pointers) {
      def secureIndex(i:Int):Int = {
        if (i < 0) cups.length-1+((i+1)%cups.length) else i % cups.length
      }
      def cupAt(i:Int):Int = cups(secureIndex(i))
      def cup:Cup = cups(current)
      def destCup:Cup = {
        @tailrec def check(thatCup:Int):Int = {
          if (thatCup == 0) check(cups.length) else {
            if (pointers(thatCup) > 3 ) thatCup
            else check(thatCup - 1)
          }
        }
        check(cup - 1)
      }

      def result:Long = {
        val cup1index = pointers(1)
        val next = cupAt(cup1index+1)
        val prev = cupAt(cup1index-1)
        next.toLong*prev
      }

      def resultStar1():String = {
        cups.splitAt(cups.indexWhere(_ == 1)) match {
          case (left, right) => right.tail.mkString + left.mkString
        }
      }


      @tailrec
      final def play(moves:Int):GameState = {
        if (moves == 0) this else {
          val fromIndex = pointers(cup)
          val toIndex = pointers(destCup)
          val a = cupAt(fromIndex+1)
          val b = cupAt(fromIndex+2)
          val c = cupAt(fromIndex+3)

          if (fromIndex < toIndex) {
            val to = cupAt(toIndex)
            for {i <- toIndex.to(fromIndex+2).by(-1)} {
              val si = secureIndex(i+1)
              cups(si) = cupAt(i)
              pointers(cupAt(i)) = si
            }
            cups(secureIndex(fromIndex+1)) = to
            pointers(to) = secureIndex(fromIndex+1)
          } else {
            for {i <- fromIndex+3 to fromIndex+4 by -1} {
              val si = secureIndex(i-1)
              cups(si) = cupAt(i)
              pointers(cupAt(i)) = si
            }
            cups(toIndex+1) = a
            cups(toIndex+2) = b
            cups(toIndex+3) = c
            pointers(a) = toIndex+1
            pointers(b) = toIndex+2
            pointers(c) = toIndex+3
          }

          GameState(secureIndex(current+1), cups, pointers).play(moves - 1)
        }
      }

      override def toString() = cups.mkString+s" $cup->$destCup"
    }



    def solve(input: String, moves:Int=1_000_000, maxCup:Int=1_000_000): Long = {
      val cups        = input.trim.split("").map(_.toInt).to(Array)
      val pointers    = (-1)+:cups.map(cup => cups.indexWhere(_ == cup)) // The first pointer is never used as 0 is not a valid value for cups
      val allCups     = cups++:10.to(maxCup).to(Array)
      val allPointers = pointers++:10.to(maxCup).map(_ - 1).to(Array)
      val state = GameState(0, allCups, allPointers)
      val lastState = state.play(moves)
      lastState.result
    }

    def solveStar1(input: String): String = {
      val cups        = input.trim.split("").map(_.toInt).to(Array)
      val pointers    = (-1)+:cups.map(cup => cups.indexWhere(_ == cup)) // The first pointer is never used as 0 is not a valid value for cups
      val state = GameState(0, cups, pointers)
      val lastState = state.play(100)
      lastState.resultStar1()
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

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay23.Part2._
    //solve("389125467") shouldBe 149245887792L
  }
//  it should "still work with star#1 - 1" in {
//    import PuzzleDay23.Part2._
//    solveStar1("389125467") shouldBe "67384529"
//  }
//  it should "still work with star#1 - 2" in {
//    import PuzzleDay23.Part2._
//    solve("389547612") shouldBe "45286397"
//  }
  it should "give the right result on the input file" in {
    import PuzzleDay23.Part2._
    solve("389547612") shouldBe 0L
  }
}

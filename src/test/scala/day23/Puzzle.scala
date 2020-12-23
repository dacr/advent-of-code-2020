package day23

// solved in XXmn

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.math._
import scala.annotation.tailrec
import scala.collection.immutable.Queue

object PuzzleDay23 {

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

  object Part1 {

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

    def solve(input: String): Long = {
      ???
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

  "puzzle star#2 example" should "give the right result on the example" ignore {
    import PuzzleDay23.Part2._
    solve(resourceContent("day23/input-example-1.txt")) shouldBe 291
  }
  it should "give the right result on the input file" ignore {
    import PuzzleDay23.Part2._
    solve(resourceContent("day23/input-given-1.txt")) shouldBe 34771
  }
}

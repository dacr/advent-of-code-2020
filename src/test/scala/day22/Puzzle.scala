package day22

// solved in XXmn

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.math._
import scala.annotation.tailrec
import scala.collection.immutable.Queue

object PuzzleDay22 {

  type GameState=List[Queue[Long]]

  def parse(input:String):GameState= {
    input
      .split("\n\n")
      .to(List)
      .map(_.split(":\n",2))
      .collect {case Array(_,cards)=> cards.split("\n").to(Queue)}
      .map(_.map(_.toLong))
  }

  object Part1 {

    def play(state:GameState):GameState = {
      if (state.exists(_.isEmpty)) state
      else state.map(_.dequeue) match {
        case (card0,remain0)::(card1,remain1)::Nil if card0 > card1 =>
          play(List(remain0.enqueue(card0).enqueue(card1),remain1))
        case (card0,remain0)::(card1,remain1)::Nil if card0 < card1 =>
          play(List(remain0,remain1.enqueue(card1).enqueue(card0)))
      }
    }

    def solve(input: String): Long = {
      val state = parse(input)
      val endState = play(state)
      endState.find(_.nonEmpty) match {
        case Some(queue) => queue.zipWithIndex.map{case (card, index) => card * (queue.size-index)}.sum
        case None => throw new RuntimeException("Impossible")
      }
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

class PuzzleDay22Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay22.Part1._
    solve(resourceContent("day22/input-example-1.txt")) shouldBe 306
  }
  it should "give the right result on the input file" in {
    import PuzzleDay22.Part1._
    solve(resourceContent("day22/input-given-1.txt")) shouldBe 35818
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" ignore {
    import PuzzleDay22.Part2._
    solve(resourceContent("day22/input-example-1.txt")) shouldBe -1
  }
  it should "give the right result on the input file" ignore {
    import PuzzleDay22.Part2._
    solve(resourceContent("day22/input-given-1.txt")) shouldBe -1
  }
}

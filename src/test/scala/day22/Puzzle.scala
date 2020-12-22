package day22

// solved in XXmn

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.math._
import scala.annotation.tailrec
import scala.collection.immutable.Queue

object PuzzleDay22 {

  type GameState=List[Queue[Int]]

  def parse(input:String):GameState= {
    input
      .split("\n\n")
      .to(List)
      .map(_.split(":\n",2))
      .collect {case Array(_,cards)=> cards.split("\n").to(Queue)}
      .map(_.map(_.toInt))
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

    def play(round:Int, state:GameState, playedState:Set[GameState]):GameState = {
      //println(round, state)
      if (playedState.exists(_.head == state.head)) List(state.head, Queue.empty) // in that case player 1 always win
      else if (playedState.exists(_.tail.head == state.tail.head)) List(state.head, Queue.empty) // in that case player 1 always win
      else if (state.exists(_.isEmpty)) state //
      else state.map(_.dequeue) match {
        case (card1,remains1)::(card2,remains2)::Nil if card1 <= remains1.size && card2 <= remains2.size =>
          val subState = List(remains1.take(card1), remains2.take(card2))
          val subEndState = play(1, subState, Set.empty)
          if (subEndState.head.nonEmpty) { // 1 win
            play(round+1, List(remains1.enqueue(card1).enqueue(card2),remains2), playedState+state)
          } else { // 2 win
            play(round+1, List(remains1,remains2.enqueue(card2).enqueue(card1)), playedState+state)
          }
        case (card1,remains1)::(card2,remains2)::Nil if card1 > card2 => // Player 1 win
          play(round+1, List(remains1.enqueue(card1).enqueue(card2),remains2), playedState+state)
        case (card1,remains1)::(card2,remains2)::Nil if card1 < card2 => // Player 2 win
          play(round+1, List(remains1,remains2.enqueue(card2).enqueue(card1)), playedState+state)
      }
    }


    def solve(input: String): Long = {
      val state = parse(input)
      val endState = play(1, state, Set.empty)
      endState.find(_.nonEmpty) match {
        case Some(queue) => queue.zipWithIndex.map{case (card, index) => card * (queue.size-index)}.sum
        case None => throw new RuntimeException("Impossible")
      }
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

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay22.Part2._
    solve(resourceContent("day22/input-example-1.txt")) shouldBe 291
  }
  it should "detect infinite conditions" in {
    import PuzzleDay22.Part2._
    val content =
      """Player 1:
        |43
        |19
        |
        |Player 2:
        |2
        |29
        |14""".stripMargin
    solve(content) shouldBe 105
  }
  it should "give the right result on the input file" in {
    import PuzzleDay22.Part2._
    solve(resourceContent("day22/input-given-1.txt")) shouldBe 34771
  }
}

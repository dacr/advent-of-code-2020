package day8

// SOLVED IN 1h15m !!

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.annotation.tailrec

object PuzzleDay8 {

  object Part1 {

    case class State(accumulator:Int, pointer:Int, history:List[Int]=Nil)

    val instrRE="""(\w+) ([+-]\d+)""".r
    sealed trait Instr {
      def value:Int
      def exec(state:State):State
    }
    case class Nop(value:Int) extends Instr {
      override def exec(state:State):State =
        state.copy(
          pointer = state.pointer + 1,
          history=state.pointer::state.history
        )
    }
    case class Acc(value:Int) extends Instr {
      override def exec(state:State):State =
        state.copy(
          accumulator = state.accumulator + value,
          pointer = state.pointer + 1,
          history=state.pointer::state.history
        )
    }
    case class Jmp(value:Int) extends Instr {
      override def exec(state:State):State =
        state.copy(
          pointer = state.pointer + value,
          history=state.pointer::state.history
        )
    }

    def parse(input:String):Array[Instr] = {
      input
        .split("\n")
        .collect{
          case instrRE(code, value) if code == "nop" => Nop(value.toInt)
          case instrRE(code, value) if code == "acc" => Acc(value.toInt)
          case instrRE(code, value) if code == "jmp" => Jmp(value.toInt)
        }
    }

    def executor(instrs: Array[Instr], state: State):(State, String) = {
      if (state.pointer >= instrs.length) (state, "aborted")
      else {
        val newState = instrs(state.pointer).exec(state)
        val runningState = {
          if (newState.history.contains(newState.pointer)) "aborted"
          else if (newState.pointer == instrs.size) "finished"
          else "running"
        }
        (newState, runningState)
      }
    }


    def solve(input: String): Int = {
      val instructions = parse(input)

      var state = State(0,0)
      var continue = true
      while(continue) {
        val (newState, runningState) = executor(instructions, state)
        if (runningState != "running") continue=false
        else state = newState
      }
      state.accumulator
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {
    import Part1._

    def testRun(instructions:Array[Instr]):Option[Int] = {
      var state = State(0,0)
      var continue = true
      var result = Option.empty[Int]
      while(continue) {
        val (newState, runningState) = executor(instructions, state)
        if (runningState == "running") state = newState
        else if (runningState == "aborted") {
          continue = false
        } else { // "finished"
          continue = false
          result = Some(newState.accumulator)
        }
      }
      result
    }

    def solve2(input: String): Int = {
      val instructions = parse(input)
      var i = 0
      var result = 0
      var continue = true
      while(continue) { // && i < instructions.length
        instructions(i) match {
          case Nop(v) =>
            testRun(instructions.updated(i, Jmp(v))) match {
              case Some(found) => result=found ; continue = false
              case None =>
            }
          case Jmp(v) =>
            testRun(instructions.updated(i, Nop(v))) match {
              case Some(found) => result=found ; continue = false
              case None =>
            }
          case _ =>
        }
        i+=1
      }
      result
    }
  }

}

// =====================================================================================

class PuzzleDay8Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay8.Part1._
    solve(resourceContent("day8/input-example-1.txt")) shouldBe 5
  }
  it should "give the right result on the input file" in {
    import PuzzleDay8.Part1._
    solve(resourceContent("day8/input-given-1.txt")) shouldBe 1451
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay8.Part2._
    solve2(resourceContent("day8/input-example-1.txt")) shouldBe 8
  }
  it should "give the right result on the input file" in {
    import PuzzleDay8.Part2._
    solve2(resourceContent("day8/input-given-1.txt")) shouldBe 1160
  }

}

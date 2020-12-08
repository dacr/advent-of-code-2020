package day8

// SOLVED IN 1h15m !! And then greatly refactored after while

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.annotation.tailrec

object PuzzleDay8 {

  object Part1 {

    sealed trait Instr {
      def value:Int
    }
    case class Nop(value:Int) extends Instr
    case class Acc(value:Int) extends Instr
    case class Jmp(value:Int) extends Instr

    sealed trait RunState
    object Aborted extends RunState
    object Runnable extends RunState
    object Finished extends RunState


    case class RunContext(
      instructions:Array[Instr],
      accumulator:Int=0,
      pointer:Int=0,
      history:List[Int]=Nil
    ) {
      def step():RunContext = {
          instructions(pointer) match {
            case Nop(_) =>
              copy(
                pointer = pointer + 1,
                history=pointer::history,
              )
            case Acc(value) =>
              copy(
                pointer = pointer + 1,
                history=pointer::history,
                accumulator = accumulator + value,
              )
            case Jmp(value) =>
              copy(
                pointer = pointer + value,
                history = pointer :: history,
              )
          }
      }

      def state():RunState = {
        if (history.contains(pointer)) Aborted
        else if (pointer == instructions.length) Finished
        else Runnable
      }

      def run():RunContext = {
        if (state()==Runnable) step().run() else this
      }
    }

    // ----------------------------------------------------------------------------------------------------------

    val instrRE="""(\w+) ([+-]\d+)""".r

    def parse(input:String):Array[Instr] = {
      input
        .split("\n")
        .collect{
          case instrRE(code, value) if code == "nop" => Nop(value.toInt)
          case instrRE(code, value) if code == "acc" => Acc(value.toInt)
          case instrRE(code, value) if code == "jmp" => Jmp(value.toInt)
        }
    }

    // ----------------------------------------------------------------------------------------------------------


    def solve(input: String): Int = {
      val instructions = parse(input)
      RunContext(instructions).run().accumulator
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {
    import Part1._

    def solve2(input: String): Int = {
      val instructions = parse(input)
      instructions
        .zipWithIndex
        .to(LazyList)
        .collect {
          case (Nop(v), index) => instructions.updated(index, Jmp(v))
          case (Jmp(v), index) => instructions.updated(index, Nop(v))
        }
        .map(instructions => RunContext(instructions))
        .map(_.run())
        .find(_.state() == Finished)
        .map(_.accumulator)
        .getOrElse(-1)
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

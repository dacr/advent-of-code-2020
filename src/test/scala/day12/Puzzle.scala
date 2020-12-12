package day12

// SOLVED 1h30m but directly without mutations or variables

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.math._

import scala.annotation.tailrec

object PuzzleDay12 {

  object Part1 {

    val rotRight = Map(
      'E' -> 'S',
      'S' -> 'W',
      'W' -> 'N',
      'N' -> 'E'
    )
    val rotLeft = rotRight.map{case (k,v)=> v->k}

    def rotateLeft(from: Char, angle: Int): Char = {
      Array.iterate(from, 1 + angle / 90)(rotLeft).last
    }

    def rotateRight(from: Char, angle: Int): Char = {
      Array.iterate(from, 1 + angle / 90)(rotRight).last
    }

    def solve(input: Iterable[String]): Long = {
      input
        .map { l => (l.head, l.tail.toInt) }
        .foldLeft(('E', 0, 0)) { case ((direction, w, h), next) =>
          (direction, next) match {
            case (d, ('N', l)) => (d, w, h + l)
            case (d, ('S', l)) => (d, w, h - l)
            case (d, ('E', l)) => (d, w + l, h)
            case (d, ('W', l)) => (d, w - l, h)
            case (d, ('L', l)) => (rotateLeft(d, l), w, h)
            case (d, ('R', l)) => (rotateRight(d, l), w, h)
            case ('E', ('F', l)) => (direction, w + l, h)
            case ('W', ('F', l)) => (direction, w - l, h)
            case ('N', ('F', l)) => (direction, w, h + l)
            case ('S', ('F', l)) => (direction, w, h - l)
          }
        } match {
        case (direction, w, h) =>
          abs(w) + abs(h)
      }
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {

    import Part1._


    def rotateLeft(l: Int, x: Int, y: Int): (Int, Int) = {
      (l / 90) % 4 match {
        case 0 => (x, y)
        case 1 => (-y, x)
        case 2 => (-x, -y)
        case 3 => (y, -x)
      }
    }

    def rotateRight(l: Int, x: Int, y: Int): (Int, Int) = {
      (l / 90) % 4 match {
        case 0 => (x, y)
        case 1 => (y, -x)
        case 2 => (-x, -y)
        case 3 => (-y, x)

      }
    }

    def solve2(input: Iterable[String]): Long = {
      input
        .map { l => (l.head, l.tail.toInt) }
        .foldLeft(((0, 0), (10, 1))) { case (((bx, by), (wpx, wpy)), next) =>
          next match {
            case ('N', l) => ((bx, by), (wpx, wpy + l))
            case ('S', l) => ((bx, by), (wpx, wpy - l))
            case ('E', l) => ((bx, by), (wpx + l, wpy))
            case ('W', l) => ((bx, by), (wpx - l, wpy))
            case ('L', l) => ((bx, by), rotateLeft(l, wpx, wpy))
            case ('R', l) => ((bx, by), rotateRight(l, wpx, wpy))
            case ('F', l) => ((bx + wpx * l, by + wpy * l), (wpx, wpy))
          }
        } match {
        case ((bx, by), _) => abs(bx) + abs(by)
      }
    }

  }

}

// =====================================================================================

class PuzzleDay12Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay12.Part1._
    solve(resourceContentLines("day12/input-example-1.txt")) shouldBe 25
  }
  it should "give the right result on the input file" in {
    import PuzzleDay12.Part1._
    solve(resourceContentLines("day12/input-given-1.txt")) should not be 1472
    solve(resourceContentLines("day12/input-given-1.txt")) shouldBe 1496
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay12.Part2._
    solve2(resourceContentLines("day12/input-example-1.txt")) shouldBe 286
  }
  it should "give the right result on the input file" in {
    import PuzzleDay12.Part2._
    solve2(resourceContentLines("day12/input-given-1.txt")) shouldBe 63843
  }

}

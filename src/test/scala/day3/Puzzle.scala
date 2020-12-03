import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

object PuzzleDay3 {

  def solve1(input: Iterable[String]): Int = {
    val land = input.toList
    val width = input.head.length
    val (x, y) = (0, 0)
    val (xv, yv) = (3, 1)
    land
      .foldLeft((0, 0, 0)) {
        case ((xc, yc, count), row) if (row(xc % width) == '#') => (xc + xv, yc + yv, count + 1)
        case ((xc, yc, count), row) => (xc + xv, yc + yv, count)
      } match {
      case (xc, yc, count) => count
    }
  }

  // -------------------------------------------------------------------------
  def countSlope(land: List[String])(increments: (Int, Int)): Long = {
    val width = land.head.length
    val (x, y) = (0, 0)
    val (xv, yv) = increments
    land // TODO - IN FACT probably not the best solution from understandability point of view
      .foldLeft((0, 0, 0L)) {
        case ((xc, yc, count), row) if yc % yv != 0 => (xc, yc + 1, count)
        case ((xc, yc, count), row) if row(xc % width) == '#'  => (xc + xv, yc + 1, count + 1)
        case ((xc, yc, count), row) => (xc + xv, yc + 1, count)
      } match {
      case (xc, yc, count) => count
    }
  }

  def solve2(input: Iterable[String]): Long = {
    val land = input.toList
    val slopes = List(
      (1, 1),
      (3, 1),
      (5, 1),
      (7, 1),
      (1, 2)
    )
    slopes.map(countSlope(land)).reduce(_ * _)
  }
}

class PuzzleDay3Test extends AnyFlatSpec with should.Matchers with Helpers {

  import PuzzleDay3._

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    solve1(resourceContent("day3/input-example-1.txt")) shouldBe 7
  }

  it should "give the right answer on the given file" in {
    solve1(resourceContent("day3/input-given-1.txt")) shouldBe 276
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    solve2(resourceContent("day3/input-example-1.txt")) shouldBe 336
  }

  it should "give the right answer on the given file" in {
    solve2(resourceContent("day3/input-given-1.txt")) should not be 21114000000L
    solve2(resourceContent("day3/input-given-1.txt")) shouldBe 7812180000L
  }

}

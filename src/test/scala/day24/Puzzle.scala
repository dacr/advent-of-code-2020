package day24

// solved in XXmn

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.math._
import scala.annotation.tailrec
import scala.collection.immutable.Queue

/*

https://www.redblobgames.com/grids/hexagons/

 */




object PuzzleDay24 {
  type Direction=String

  case class OffsetCoord(q:Int, r:Int)
  case class CubeCoord(x:Int, y:Int, z:Int) {
    def add(that:CubeCoord):CubeCoord = CubeCoord(x+that.x, y+that.y, z+that.z)
  }

  val moves:Map[Direction,CubeCoord] = Map(
    //"n"  -> CubeCoord(+0,+0,-1),
    //"s"  -> CubeCoord(+0,+0,+1),
    "e"  -> CubeCoord(+1,-1,+0),
    "w"  -> CubeCoord(-1,+1,+0),
    "nw" -> CubeCoord(+0,+1,+0),
    "se" -> CubeCoord(+0,-1,+0),
    "ne" -> CubeCoord(+1,+0,+0),
    "sw" -> CubeCoord(-1,+0,+0),
  )

  // convert odd-r offset to cube
  implicit def offset2cube(o:OffsetCoord):CubeCoord = {
    val x = o.q-(o.r - (o.r&1))/2
    val z = o.r
    val y = -x-z
    CubeCoord(x, y, z)
  }
  // convert cube to odd-r offset
  implicit def cube2offset(c:CubeCoord):OffsetCoord = {
    val q = c.x + (c.z-(c.z&1)) / 2
    val r = c.z
    OffsetCoord(q,r)
  }

  def distance(a:CubeCoord, b:CubeCoord):Int = {
    (abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z)) / 2
  }

  def parseDirections(input:String):Vector[CubeCoord] = {
    def worker(remain:String, accu:Vector[CubeCoord]):Vector[CubeCoord] = {
      remain match {
        case "" => accu
        case s"ne$rem" => worker(rem, accu:+moves("ne"))
        case s"nw$rem" => worker(rem, accu:+moves("nw"))
        case s"se$rem" => worker(rem, accu:+moves("se"))
        case s"sw$rem" => worker(rem, accu:+moves("sw"))
        //case s"n$rem" => worker(rem, accu:+moves("n"))
        //case s"s$rem" => worker(rem, accu:+moves("s"))
        case s"e$rem" => worker(rem, accu:+moves("e"))
        case s"w$rem" => worker(rem, accu:+moves("w"))
      }
    }
    worker(input, Vector.empty)
  }

  def parse(input:String):List[Vector[CubeCoord]] = {
    input.split("\n").map(parseDirections).toList
  }

  sealed trait Color
  object White extends Color
  object Black extends Color

  type Grid=Map[CubeCoord, Color]


  @tailrec
  def fillGrid(remainingMoves:List[Vector[CubeCoord]], grid:Grid):Grid = {
    remainingMoves match {
      case Nil => grid
      case moves::newRemainingMoves =>
        val cube = moves.reduce(_ add _)
        val color = grid.getOrElse(cube, White) match {
          case White => Black
          case Black => White
        }
        val updatedGrid = grid.updated(cube, color)
        fillGrid(newRemainingMoves, updatedGrid)
    }
  }

  // =============================================================================

  object Part1 {

    def solve(input: String): Long = {
      val moves = parse(input)
      val grid = fillGrid(moves, Map.empty)
      grid.values.count(_ == Black)
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {

    //
    def generateWhiteTilesAroundBlackTiles(grid: Grid):Grid = {
      def fillWhite(remainingBlacks:List[CubeCoord], grid:Grid):Grid = {
        remainingBlacks match {
          case Nil => grid
          case coord::rems =>
            val neighbors = moves.values.map(deltaCoord => coord.add(deltaCoord))
            val whites = neighbors.filterNot(grid.contains).map(coord => coord -> White)
            val updatedGrid = grid++whites
            fillWhite(rems, updatedGrid)
        }
      }
      val blacks = grid.collect{case (cube,Black) => cube}.toList
      fillWhite(blacks, grid)
    }

    @tailrec
    def afterGenerations(value: Int, grid: Grid):Grid = {
      if (value == 0) grid else {
        val newGrid = grid map {
          case (coord, White) =>
            val neighbors = moves.values.map(deltaCoord => coord.add(deltaCoord))
            val count = neighbors.count(coord => grid.getOrElse(coord, White) == Black)
            coord -> (if (count==2) Black else White)
          case (coord, Black) =>
            val neighbors = moves.values.map(deltaCoord => coord.add(deltaCoord))
            val count = neighbors.count(coord => grid.getOrElse(coord, White) == Black)
            coord -> (if (count==0 || count>2) White else Black)
        }
        afterGenerations(value - 1, generateWhiteTilesAroundBlackTiles(newGrid))
      }
    }

    def solve(input: String): Long = {
      val moves = parse(input)
      val initial = generateWhiteTilesAroundBlackTiles(fillGrid(moves, Map.empty))
      val grid = afterGenerations(100, initial)
      grid.values.count(_ == Black)
    }

  }

}

// =====================================================================================

class PuzzleDay24Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay24.Part1._
    solve(resourceContent("day24/input-example-1.txt")) shouldBe 10
  }
  it should "give the right result on the input file" in {
    import PuzzleDay24.Part1._
    solve(resourceContent("day24/input-given-1.txt")) shouldBe 375
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay24.Part2._
    solve(resourceContent("day24/input-example-1.txt")) shouldBe 2208
  }
  it should "give the right result on the input file" in {
    import PuzzleDay24.Part2._
    solve(resourceContent("day24/input-given-1.txt")) shouldBe 3937
  }
}

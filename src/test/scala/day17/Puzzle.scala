package day17

// solved in 45mn !

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.math._

import scala.annotation.tailrec

object PuzzleDay17 {


  object Part1 {

    case class Coord(x: Int, y: Int, z: Int) {
      def add(that: Coord): Coord = Coord(x + that.x, y + that.y, z + that.z)
    }

    val adjacents: Seq[Coord] = for {
      dx <- -1 to 1
      dy <- -1 to 1
      dz <- -1 to 1
      if !(dx == 0 && dy == 0 && dz == 0)
    } yield Coord(dx, dy, dz)

    val InactiveCube = '.'
    val ActiveCube = '#'

    type Board = Map[Coord, Char]

    def buildBoard(input: String): Board = {
      val z = 0
      val cubes = for {
        (row, y) <- input.split("\n").zipWithIndex
        (col, x) <- row.toCharArray.zipWithIndex
      } yield {
        Coord(x, y, z) -> col
      }
      cubes.toMap
    }

    def boardTestCoords(board: Board): Iterable[Coord] = {
      val minX = board.keys.minBy(_.x).x
      val maxX = board.keys.maxBy(_.x).x
      val minY = board.keys.minBy(_.y).y
      val maxY = board.keys.maxBy(_.y).y
      val minZ = board.keys.minBy(_.z).z
      val maxZ = board.keys.maxBy(_.z).z
      val coords = for {
        z <- minZ - 1 to maxZ + 1
        y <- minY - 1 to maxY + 1
        x <- minX - 1 to maxX + 1
      } yield Coord(x, y, z)
      coords
    }


    def solve(input: String): Int = {
      val initialBoard = buildBoard(input)

      def worker(board: Board, round: Int): Board = {
        if (round == 6) board
        else {
          val newBoard =
            boardTestCoords(board).map { coord =>
              val cube = board.getOrElse(coord, InactiveCube)
              val activeAdjacents = adjacents.count(c => board.getOrElse(coord.add(c), InactiveCube) == ActiveCube)
              cube match {
                case ActiveCube =>
                  val state = if (activeAdjacents == 2 || activeAdjacents == 3) ActiveCube else InactiveCube
                  coord -> state
                case InactiveCube =>
                  val state = if (activeAdjacents == 3) ActiveCube else InactiveCube
                  coord -> state
              }
            }
          worker(newBoard.toMap, round + 1)
        }
      }

      val lastBoard = worker(initialBoard, 0)
      lastBoard.values.count(_ == ActiveCube)
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {

    case class Coord(x: Int, y: Int, z: Int, w:Int) {
      def add(that: Coord): Coord = Coord(x + that.x, y + that.y, z + that.z, w+that.w)
    }

    val adjacents: Seq[Coord] = for {
      dx <- -1 to 1
      dy <- -1 to 1
      dz <- -1 to 1
      dw <- -1 to 1
      if !(dx == 0 && dy == 0 && dz == 0 && dw ==0)
    } yield Coord(dx, dy, dz, dw)

    val InactiveCube = '.'
    val ActiveCube = '#'

    type Board = Map[Coord, Char]

    def buildBoard(input: String): Board = {
      val z = 0
      val w = 0
      val cubes = for {
        (row, y) <- input.split("\n").zipWithIndex
        (col, x) <- row.toCharArray.zipWithIndex
      } yield {
        Coord(x, y, z, w) -> col
      }
      cubes.toMap
    }

    def boardTestCoords(board: Board): Iterable[Coord] = {
      val minX = board.keys.minBy(_.x).x
      val maxX = board.keys.maxBy(_.x).x
      val minY = board.keys.minBy(_.y).y
      val maxY = board.keys.maxBy(_.y).y
      val minZ = board.keys.minBy(_.z).z
      val maxZ = board.keys.maxBy(_.z).z
      val minW = board.keys.minBy(_.w).w
      val maxW = board.keys.maxBy(_.w).w
      val coords = for {
        w <- minW - 1 to maxW + 1
        z <- minZ - 1 to maxZ + 1
        y <- minY - 1 to maxY + 1
        x <- minX - 1 to maxX + 1
      } yield Coord(x, y, z, w)
      coords
    }


    def solve(input: String): Int = {
      val initialBoard = buildBoard(input)

      def worker(board: Board, round: Int): Board = {
        if (round == 6) board
        else {
          val newBoard =
            boardTestCoords(board).map { coord =>
              val cube = board.getOrElse(coord, InactiveCube)
              val activeAdjacents = adjacents.count(c => board.getOrElse(coord.add(c), InactiveCube) == ActiveCube)
              cube match {
                case ActiveCube =>
                  val state = if (activeAdjacents == 2 || activeAdjacents == 3) ActiveCube else InactiveCube
                  coord -> state
                case InactiveCube =>
                  val state = if (activeAdjacents == 3) ActiveCube else InactiveCube
                  coord -> state
              }
            }
          worker(newBoard.toMap, round + 1)
        }
      }

      val lastBoard = worker(initialBoard, 0)
      lastBoard.values.count(_ == ActiveCube)
    }

  }

}

// =====================================================================================

class PuzzleDay17Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay17.Part1._
    solve(resourceContent("day17/input-example-1.txt")) shouldBe 112
  }
  it should "give the right result on the input file" in {
    import PuzzleDay17.Part1._
    solve(resourceContent("day17/input-given-1.txt")) shouldBe 310
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay17.Part2._
    solve(resourceContent("day17/input-example-1.txt")) shouldBe 848
  }
  it should "give the right result on the input file" in {
    import PuzzleDay17.Part2._
    solve(resourceContent("day17/input-given-1.txt")) shouldBe 2056
  }

}

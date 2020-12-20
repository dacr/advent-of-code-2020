package day20

// solved in XXmn

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.OptionValues._

import java.util
import java.util.regex.Pattern
import scala.math._
import scala.annotation.tailrec
import scala.util.matching.Regex

object PuzzleDay20 {

  type Cells = List[Int]
  type Tiles = List[Tile]

  def pow2(p: Int): Int = (1 << p)

  def rightBorder(cells: Cells): Int = cells.zipWithIndex.map { case (cell, index) => ((cell & pow2(0)) >> 0) << (cells.size - 1 - index) }.reduce(_ | _)

  def leftBorder(cells: Cells): Int = cells.zipWithIndex.map { case (cell, index) => ((cell & pow2(cells.size - 1)) >> cells.size - 1) << (cells.size - 1 - index) }.reduce(_ | _)

  def flipBits(in: Int, bitsCount: Int = 10): Int = (0 until bitsCount).foldLeft(0) { case (out, n) => out | (((in & pow2(n)) >> n) << (bitsCount - 1 - n)) }

  case class Borders(up: Int, down: Int, left: Int, right: Int) {
    def hflip = Borders(up = flipBits(up), down = flipBits(down), left = right, right = left)

    def vflip = Borders(up = down, down = up, left = flipBits(left), right = flipBits(right))

    def rotR = Borders(up = flipBits(left), down = flipBits(right), left = down, right = up)

    def rotL = Borders(up = right, down = left, left = flipBits(up), right = flipBits(down))

    def rot2 = Borders(up = flipBits(down), down = flipBits(up), left = flipBits(right), right = flipBits(left))
  }

  case class Tile(id: Long, borders: Borders) {
    val possibleBordersFor = List(
      borders,
      borders.hflip,
      borders.vflip,
      borders.rotR,
      borders.rotL,
      borders.rot2
    )
  }

  def parseTile(input: String): Tile = {
    input.split(":", 2) match {
      case Array(s"Tile $ids", tileSpec) =>
        val cells =
          tileSpec
            .split("\n")
            .map(_.replaceAll("[#]", "1").replaceAll("[.]", "0").trim)
            .filter(_.size > 0)
            .map(s => java.lang.Integer.parseInt(s, 2))
            .toList
        val borders = Borders(up = cells.head, down = cells.last, left = leftBorder(cells), right = rightBorder(cells))
        Tile(ids.toInt, borders)
    }
  }

  def parse(input: String): Tiles = {
    input
      .split("\n\n")
      .toList
      .map(_.trim)
      .filter(_.size > 0)
      .map(parseTile)
  }

  object Part1 {

    def checkAttachableTiles(ax: Int, ay: Int, a: Borders, bx: Int, by: Int, b: Borders): Boolean = {
      if (ax < bx) a.right == b.left
      else if (ax > bx) a.left == b.right
      else if (ay < by) a.down == b.up
      else if (ay > by) a.up == b.down
      else throw new RuntimeException("Impossible)")
    }

    def buildCheckSquare(geoAccu: Map[(Int, Int), Tile], sideSize: Int): Option[Array[Tile]] = {
      val minx = geoAccu.keys.map { case (x, _) => x }.min
      val miny = geoAccu.keys.map { case (_, y) => y }.min
      val maxx = geoAccu.keys.map { case (x, _) => x }.max
      val maxy = geoAccu.keys.map { case (_, y) => y }.max
      if (maxx - minx == sideSize - 1 && maxy - miny == sideSize - 1) {
        val result = {
          for {
            y <- miny to maxy
            x <- minx to maxx
            found = geoAccu((x, y))
          } yield found
        }
        Some(result.toArray)
      } else None
    }

    def checkGeoMap(geoAccu: Map[(Int, Int), Tile], sideSize: Int): Boolean = {
      if (geoAccu.isEmpty) true else {
        val minx = geoAccu.keys.map { case (x, _) => x }.min
        val miny = geoAccu.keys.map { case (_, y) => y }.min
        val maxx = geoAccu.keys.map { case (x, _) => x }.max
        val maxy = geoAccu.keys.map { case (_, y) => y }.max

        maxx - minx <= sideSize - 1 && maxy - miny <= sideSize - 1
      }
    }

    def search(tiles: Tiles, sideSize: Int): Option[Array[Tile]] = {
      def worker(remainingTiles: Tiles, geoAccu: Map[(Int, Int), Tile]): Option[Array[Tile]] = {
        remainingTiles match {
          case _ if !checkGeoMap(geoAccu, sideSize) => None
          case Nil =>
            buildCheckSquare(geoAccu, sideSize)
          case current :: tail if geoAccu.isEmpty =>
            val id = current.id
            val possibleBorders = current.possibleBordersFor.to(LazyList)
            possibleBorders.flatMap { border =>
              worker(tail, Map((0, 0) -> Tile(id, border)))
            }.headOption
          case current :: tail =>
            val id = current.id
            val possibleBorders = current.possibleBordersFor.to(LazyList)
            val attachables: LazyList[((Int, Int), Tile)] =
              for {
                borders <- possibleBorders
                ((x, y), existing) <- geoAccu // check already attached coords
                (nx, ny) <- List((1, 0), (-1, 0), (0, 1), (0, -1)).map { case (dx, dy) => (x + dx, y + dy) } // positions candidates
                if !geoAccu.contains((nx, ny)) // check if position candidate is free
                arounds = List((1, 0), (-1, 0), (0, 1), (0, -1)).map { case (dx, dy) => (nx + dx, ny + dy) } // positions around current candidate
                neighbors = arounds.filter(geoAccu.contains)
                if neighbors.forall { case (ax, ay) => checkAttachableTiles(nx, ny, borders, ax, ay, geoAccu((ax, ay)).borders) }
              } yield {
                (nx, ny) -> Tile(id, borders)
              }
            attachables.flatMap { attachable =>
              worker(tail, geoAccu + attachable)
            }.headOption
        }
      }

      val found = worker(tiles, Map.empty)
      found
    }

    def solve(input: String): Option[Long] = {
      val tiles = parse(input)
      val sideSize = math.sqrt(tiles.length).toInt
      val found = search(tiles, sideSize)

      found.foreach { result =>
        for {r <- 0 until sideSize} {
          for {c <- 0 until sideSize} {
            print(result(r * sideSize + c).id)
            print(" ")
          }
          println()
        }
      }

      found
        .map(tiles =>
          tiles(0).id *
            tiles(sideSize - 1).id *
            tiles(sideSize * (sideSize - 1)).id *
            tiles(sideSize * sideSize - 1).id)
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

class PuzzleDay20Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay20.Part1._
    solve(resourceContent("day20/input-example-1.txt")).value shouldBe 20899048083289L
  }
  it should "give the right result on the input file" in {
    import PuzzleDay20.Part1._
    solve(resourceContent("day20/input-given-1.txt")).value shouldBe -1
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay20.Part2._
    solve(resourceContent("day20/input-example-1.txt")) shouldBe -1
  }
  it should "give the right result on the input file" in {
    import PuzzleDay20.Part2._
    solve(resourceContent("day20/input-given-1.txt")) shouldBe -1
  }
}

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

  def bin2str(n: Int, bitsCount: Int): String = s"%${bitsCount}s".format(n.toBinaryString).replaceAll(" ", "0").takeRight(bitsCount)
  def str2bin(in:String):Int = java.lang.Integer.parseInt(in, 2)


  case class Borders(up: Int, down: Int, left: Int, right: Int, bitCounts:Int=10) {
    def hflip = Borders(up = flipBits(up,bitCounts), down = flipBits(down,bitCounts), left = right, right = left, bitCounts = bitCounts)

    def vflip = Borders(up = down, down = up, left = flipBits(left, bitCounts), right = flipBits(right, bitCounts), bitCounts = bitCounts)

    def rotR = Borders(up = flipBits(left, bitCounts), down = flipBits(right, bitCounts), left = down, right = up, bitCounts = bitCounts)

    def rotL = Borders(up = right, down = left, left = flipBits(up, bitCounts), right = flipBits(down, bitCounts), bitCounts = bitCounts)

    def rot2 = Borders(up = flipBits(down, bitCounts), down = flipBits(up, bitCounts), left = flipBits(right, bitCounts), right = flipBits(left, bitCounts), bitCounts = bitCounts)
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

  def parseTile(input: String, bitCounts:Int=10): Tile = {
    input.split(":", 2) match {
      case Array(s"Tile $ids", tileSpec) =>
        val cells =
          tileSpec
            .split("\n")
            .map(_.replaceAll("[#]", "1").replaceAll("[.]", "0").trim)
            .filter(_.size > 0)
            .map(s => java.lang.Integer.parseInt(s, 2))
            .toList
        val borders = Borders(up = cells.head, down = cells.last, left = leftBorder(cells), right = rightBorder(cells), bitCounts)
        Tile(ids.toInt, borders)
    }
  }

  def parse(input: String): Tiles = {
    input
      .split("\n\n")
      .toList
      .map(_.trim)
      .filter(_.size > 0)
      .map(spec => parseTile(spec))
  }

  object Part1 {

    def checkAttachableTiles(ax: Int, ay: Int, a: Borders, bx: Int, by: Int, b: Borders): Boolean = {
      if (ax < bx) a.right == b.left
      else if (ax > bx) a.left == b.right
      else if (ay < by) a.down == b.up
      else if (ay > by) a.up == b.down
      else throw new RuntimeException("Impossible)")
    }

    def dumpGeoAccu(geoAccu: Map[(Int, Int), Tile], sideSize: Int): Unit = {
      if (geoAccu.nonEmpty) {
        val minx = geoAccu.keys.map { case (x, _) => x }.min
        val miny = geoAccu.keys.map { case (_, y) => y }.min
        val maxx = geoAccu.keys.map { case (x, _) => x }.max
        val maxy = geoAccu.keys.map { case (_, y) => y }.max
        if (maxx - minx < sideSize && maxy - miny < sideSize) {
          println("-----------------------------------------------------------")
          for {
            y <- miny to maxy
            x <- minx to maxx
            found = geoAccu.get((x, y))
          } {
            println(s"$x,$y :")
            found match {
              case Some(tile) =>
                println(s"  up  =${bin2str(tile.borders.up, 10)} down =${bin2str(tile.borders.down, 10)}")
                println(s"  left=${bin2str(tile.borders.left, 10)} right=${bin2str(tile.borders.right, 10)}")
              case None =>
            }
          }
        }
      }
    }

    def buildCheckSquare(geoAccu: Map[(Int, Int), Tile], sideSize: Int): Option[Array[Tile]] = {
      lazy val minx = geoAccu.keys.map { case (x, _) => x }.min
      lazy val miny = geoAccu.keys.map { case (_, y) => y }.min
      lazy val maxx = geoAccu.keys.map { case (x, _) => x }.max
      lazy val maxy = geoAccu.keys.map { case (_, y) => y }.max
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

    val incs = List((1, 0), (-1, 0), (0, 1), (0, -1))

    def aroundsOf(x: Int, y: Int): List[(Int, Int)] = {
      incs.map { case (dx, dy) => (x + dx, y + dy) }
    }

    def neighborhoodIsChecked(ax: Int, ay: Int, borders: Borders, neighbors: List[(Int, Int)], geoAccu: Map[(Int, Int), Tile], sideSize:Int) = {
      neighbors.forall { case (nx, ny) =>
        checkAttachableTiles(ax, ay, borders, nx, ny, geoAccu((nx, ny)).borders)
      }
    }

    def search(tiles: Tiles, sideSize: Int): Option[Array[Tile]] = {
      def worker(remainingTiles: Set[Tile], geoAccu: Map[(Int, Int), Tile]): Option[Array[Tile]] = {
        if (remainingTiles.isEmpty) {
          buildCheckSquare(geoAccu, sideSize)
        } else {
          def attachables: Iterable[Option[Array[Tile]]] =
            for {
              current <- remainingTiles.to(LazyList)
              borders <- current.possibleBordersFor        // check with all possible configuration
              ((x, y), _) <- geoAccu                       // check already attached coords
              (nx, ny) <- aroundsOf(x, y)                  // positions candidates
              if !geoAccu.contains((nx, ny))               // check if position candidate is free
              arounds = aroundsOf(nx, ny)                  // positions around current candidate
              neighbors = arounds.filter(geoAccu.contains) // neighbors to check border constraints with
              if neighborhoodIsChecked(nx, ny, borders, neighbors, geoAccu, sideSize)
            } yield {
              val attachable = (nx, ny) -> Tile(current.id, borders)
              worker(remainingTiles - current, geoAccu + attachable)
            }
          attachables.collect {case Some(solution) => solution}.headOption
        }
      }

      val found = worker(tiles.tail.toSet, Map((0,0)->tiles.head))
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

  "basic functions" should "work" in {
    import PuzzleDay20._
    str2bin("10") shouldBe 2
    str2bin("100") shouldBe 4
    bin2str(4,3) shouldBe "100"
    bin2str(4,4) shouldBe "0100"
    bin2str(flipBits(str2bin("00101"), 5), 5) shouldBe "10100"
    bin2str(flipBits(str2bin("00001"), 5), 5) shouldBe "10000"
    bin2str(flipBits(str2bin("10000"), 5), 5) shouldBe "00001"
    bin2str(flipBits(str2bin("01000"), 5), 5) shouldBe "00010"
    bin2str(flipBits(str2bin("00100"), 5), 5) shouldBe "00100"
    flipBits(1,3) shouldBe 4
    flipBits(4,3) shouldBe 1
    //flipBits(bin2str)
  }

  "rotations" should "work" in {
    import PuzzleDay20._
    val borders = Borders(up=4, right=0, down=0, left=0, bitCounts=3)
    borders.rotR shouldBe Borders(up=0, right=4, down=0, left=0, bitCounts=3)
    borders.rotR.rotR shouldBe Borders(up=0, right=0, down=1, left=0, bitCounts=3)
    borders.rotR.rotR.rotR shouldBe Borders(up=0, right=0, down=0, left=1, bitCounts=3)
    borders.rotR.rotR.rotR.rotR shouldBe borders
    // ---------------
    borders.rotL shouldBe Borders(up=0, right=0, down=0, left=1, bitCounts=3)
    borders.rotL.rotL shouldBe Borders(up=0, right=0, down=1, left=0, bitCounts=3)
    borders.rotL.rotL.rotL shouldBe Borders(up=0, right=4, down=0, left=0, bitCounts=3)
    borders.rotL.rotL.rotL.rotL shouldBe borders
    // ---------------
    borders.rot2 shouldBe Borders(up=0, right=0, down=1, left=0, bitCounts=3)
    borders.rot2.rot2 shouldBe borders
  }

  "flips" should "work" in {
    import PuzzleDay20._
    Borders(up=4, right=0, down=0, left=0, bitCounts=3).hflip shouldBe Borders(up=1, right=0, down=0, left=0, bitCounts=3)
    Borders(up=0, right=0, down=1, left=0, bitCounts=3).hflip shouldBe Borders(up=0, right=0, down=4, left=0, bitCounts=3)
    // ---------------
    Borders(up=4, right=0, down=0, left=0, bitCounts=3).vflip shouldBe Borders(up=0, right=0, down=4, left=0, bitCounts=3)
    Borders(up=0, right=0, down=1, left=0, bitCounts=3).vflip shouldBe Borders(up=1, right=0, down=0, left=0, bitCounts=3)
    // ---------------
    Borders(up=1, right=1, down=0, left=0, bitCounts=3).hflip shouldBe Borders(up=4, right=0, down=0, left=1, bitCounts=3)
    Borders(up=0, right=1, down=1, left=0, bitCounts=3).hflip shouldBe Borders(up=0, right=0, down=4, left=1, bitCounts=3)
    Borders(up=2, right=1, down=1, left=0, bitCounts=3).hflip shouldBe Borders(up=2, right=0, down=4, left=1, bitCounts=3)
    Borders(up=3, right=1, down=1, left=0, bitCounts=3).hflip shouldBe Borders(up=6, right=0, down=4, left=1, bitCounts=3)
  }

  "parsing" should "work" in {
    import PuzzleDay20._
    val t1s =
      """Tile 42:
        |#..
        |#..
        |#..""".stripMargin
    val t1 = parseTile(t1s,3)
    t1.id shouldBe 42
    t1.borders shouldBe Borders(up=4, right=0, down=4, left=7, bitCounts=3)

    val t2s =
      """Tile 42:
        |#.#
        |#.#
        |#.#""".stripMargin
    val t2 = parseTile(t2s,3)
    t2.id shouldBe 42
    t2.borders shouldBe Borders(up=5, right=7, down=5, left=7, bitCounts=3)


    val t3s =
      """Tile 42:
        |#.#
        |.##
        |.#.""".stripMargin
    val t3 = parseTile(t3s,3)
    t3.id shouldBe 42
    t3.borders shouldBe Borders(up=5, right=6, down=2, left=4, bitCounts=3)
    t3.borders.rotR shouldBe Borders(up=1, right=5, down=3, left=2, bitCounts=3)
    t3.borders.rotL shouldBe Borders(up=6, right=2, down=4, left=5, bitCounts=3)
    t3.borders.hflip shouldBe Borders(up=5, right=4, down=2, left=6, bitCounts=3)
    t3.borders.vflip shouldBe Borders(up=2, right=3, down=5, left=1, bitCounts=3)
  }



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

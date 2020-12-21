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

import scala.collection.parallel.CollectionConverters._

object PuzzleDay20 {

  type Cells = List[Int]
  type Tiles = List[Tile]

  def pow2(p: Int): Int = (1 << p)

  def rightBorder(cells: Cells): Int = cells.zipWithIndex.map { case (cell, index) => ((cell & pow2(0)) >> 0) << (cells.size - 1 - index) }.reduce(_ | _)

  def leftBorder(cells: Cells): Int = cells.zipWithIndex.map { case (cell, index) => ((cell & pow2(cells.size - 1)) >> cells.size - 1) << (cells.size - 1 - index) }.reduce(_ | _)

  def flipBits(in: Int, bitsCount: Int = 10): Int = (0 until bitsCount).foldLeft(0) { case (out, n) => out | (((in & pow2(n)) >> n) << (bitsCount - 1 - n)) }

  def bin2str(n: Int, bitsCount: Int): String = s"%${bitsCount}s".format(n.toBinaryString).replaceAll(" ", "0").takeRight(bitsCount)

  def str2bin(in: String): Int = java.lang.Integer.parseInt(in, 2)


  def hflip(borders:Borders) = Borders(up = flipBits(borders.up, borders.bitCounts), down = flipBits(borders.down, borders.bitCounts), left = borders.right, right = borders.left, bitCounts = borders.bitCounts)

  def vflip(borders:Borders) = Borders(up = borders.down, down = borders.up, left = flipBits(borders.left, borders.bitCounts), right = flipBits(borders.right, borders.bitCounts), bitCounts = borders.bitCounts)

  def rotR(borders:Borders) = Borders(up = flipBits(borders.left, borders.bitCounts), down = flipBits(borders.right, borders.bitCounts), left = borders.down, right = borders.up, bitCounts = borders.bitCounts)

  def rotL(borders:Borders) = Borders(up = borders.right, down = borders.left, left = flipBits(borders.up, borders.bitCounts), right = flipBits(borders.down, borders.bitCounts), bitCounts = borders.bitCounts)

  def rot2(borders:Borders) = Borders(up = flipBits(borders.down, borders.bitCounts), down = flipBits(borders.up, borders.bitCounts), left = flipBits(borders.right, borders.bitCounts), right = flipBits(borders.left, borders.bitCounts), bitCounts = borders.bitCounts)



  case class Borders(up: Int, down: Int, left: Int, right: Int, bitCounts: Int = 10) {
    def connectors = Set(up,down,left,right)
  }

  def possibleBordersFrom(borders: Borders): List[Borders] = List(
    borders,
    hflip(borders),
    vflip(borders),
    rotR(borders),
    rotL(borders),
    rot2(borders)
  )

  case class Tile(id: Long, borders: Borders) {
    def connectors: Set[Int] = possibleBordersFrom(borders).flatMap(_.connectors).toSet
  }

  def parseTile(input: String, bitCounts: Int = 10): Tile = {
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

  def tilesGenerator(desc: String): String = {
    val numeratorStart = 2

    def decodeBigRow(bigRow: String, fromId: Int): List[String] = {
      def worker(group: Array[List[String]], id: Int, accu: List[String]): List[String] = {
        if (group.head.isEmpty) accu else {
          val tile = s"Tile $id:\n" + group.map(_.head).mkString("\n")
          worker(group.map(_.tail), id + 1, accu :+ tile)
        }
      }

      val in = bigRow.split("\n").map(_.split(" ").toList)
      worker(in, fromId, Nil)
    }

    val parts = desc.split("\n\n")
    parts.zipWithIndex.flatMap { case (bigRow, i) => decodeBigRow(bigRow, numeratorStart + i * parts.size) }.mkString("\n\n")
  }


  object Part1 {

    def solve(input: String): Long = {
      val tiles = parse(input)
      val tilesByConnector =
        tiles
          .flatMap(t => t.connectors.map(connector => connector->t))
          .groupMap{case (connector, _) => connector}{case (_,tile)=> tile}
      val tilesConnections =
        tiles.map{ tile =>
          tile -> (tile.connectors.flatMap(connector => tilesByConnector.getOrElse(connector,Nil)).map(_.id) - tile.id)
        }
      tilesConnections.collect{case (tile, connections) if connections.size==2 => tile.id}.product
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
    solve(resourceContent("day20/input-example-1.txt")) shouldBe 20899048083289L
  }
  it should "give the right result on the input file" in {
    import PuzzleDay20.Part1._
    solve(resourceContent("day20/input-given-1.txt")) shouldBe 13983397496713L
  }
  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" ignore {
    import PuzzleDay20.Part2._
    solve(resourceContent("day20/input-example-1.txt")) shouldBe -1
  }
  it should "give the right result on the input file" ignore {
    import PuzzleDay20.Part2._
    solve(resourceContent("day20/input-given-1.txt")) shouldBe -1
  }
}

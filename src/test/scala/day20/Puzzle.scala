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
  type TileId = Long
  type Connector = Int

  def pow2(p: Int): Int = (1 << p)

  def rightBorder(cells: Cells): Int = cells.zipWithIndex.map { case (cell, index) => ((cell & pow2(0)) >> 0) << (cells.size - 1 - index) }.reduce(_ | _)

  def leftBorder(cells: Cells): Int = cells.zipWithIndex.map { case (cell, index) => ((cell & pow2(cells.size - 1)) >> cells.size - 1) << (cells.size - 1 - index) }.reduce(_ | _)

  def flipBits(in: Int, bitsCount: Int = 10): Int = (0 until bitsCount).foldLeft(0) { case (out, n) => out | (((in & pow2(n)) >> n) << (bitsCount - 1 - n)) }

  def bin2str(n: Int, bitsCount: Int): String = s"%${bitsCount}s".format(n.toBinaryString).replaceAll(" ", "0").takeRight(bitsCount)

  def str2bin(in: String): Int = java.lang.Integer.parseInt(in, 2)


  def hflip(borders: Borders) = {
    Borders(
      up = flipBits(borders.up, borders.bitCounts),
      down = flipBits(borders.down, borders.bitCounts),
      left = borders.right,
      right = borders.left,
      encoding = borders.encoding.map(_.reverse),
      bitCounts = borders.bitCounts
    )
  }

  def vflip(borders: Borders) = {
    Borders(
      up = borders.down,
      down = borders.up,
      left = flipBits(borders.left, borders.bitCounts),
      right = flipBits(borders.right, borders.bitCounts),
      encoding = borders.encoding.reverse,
      bitCounts = borders.bitCounts
    )
  }

  def rotateEncodingRight(from: Array[String]): Array[String] = {
    def worker(remaining: Array[String], accu: Array[String]): Array[String] = {
      if (remaining.head.isEmpty) accu else {
        worker(remaining.map(_.tail), accu :+ remaining.map(_.head).mkString.reverse)
      }
    }

    worker(from, Array.empty)
  }

  def rotR(borders: Borders) = {
    Borders(
      up = flipBits(borders.left,
        borders.bitCounts),
      down = flipBits(borders.right, borders.bitCounts),
      left = borders.down,
      right = borders.up,
      encoding = rotateEncodingRight(borders.encoding),
      bitCounts = borders.bitCounts
    )
  }


  case class Borders(up: Int, down: Int, left: Int, right: Int, encoding: Array[String], bitCounts: Int = 10) {
    def connectors: Set[Connector] = Set(up, down, left, right)

    def id = up.toLong * 7 + down * 11 + left * 13 + right * 17
  }

  def possibleBordersFrom(borders: Borders): List[Borders] = List(
    borders,
    hflip(borders),
    vflip(borders),
    rotR(borders),
    rotR(rotR(rotR(borders))),
    rotR(rotR(borders))
  )

  case class Tile(id: TileId, borders: Borders) {
    def connectors: Set[Connector] = possibleBordersFrom(borders).flatMap(_.connectors).toSet
  }

  def parseTile(input: String, bitCounts: Int = 10): Tile = {
    input.split(":", 2) match {
      case Array(s"Tile $ids", tileSpec) =>
        val encoding =
          tileSpec
            .split("\n")
            .map(_.trim)
            .filter(_.size > 0)

        val cells =
          encoding
            .map(_.replaceAll("[#]", "1").replaceAll("[.]", "0"))
            .map(s => java.lang.Integer.parseInt(s, 2))
            .toList
        val borders = Borders(up = cells.head, down = cells.last, left = leftBorder(cells), right = rightBorder(cells), encoding, bitCounts)
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

    def solve(input: String): Long = {
      val tiles = parse(input)
      val tilesByConnector =
        tiles
          .flatMap(t => t.connectors.map(connector => connector -> t))
          .groupMap { case (connector, _) => connector } { case (_, tile) => tile }
      val tilesConnections =
        tiles.map { tile =>
          tile -> (tile.connectors.flatMap(connector => tilesByConnector.getOrElse(connector, Nil)).map(_.id) - tile.id)
        }
      tilesConnections.collect { case (tile, connections) if connections.size == 2 => tile.id }.product
    }

  }

  // -------------------------------------------------------------------------

  object Part2 {
    def solve(input: String): Long = {
      val seaMonster =
        """                  #
          |#    ##    ##    ###
          | #  #  #  #  #  #   """.stripMargin

      val tiles = parse(input)
      val size = sqrt(tiles.size).toInt
      val tilesById = tiles.map(tile => tile.id -> tile).toMap

      val tilesByConnector: Map[Connector, List[Tile]] =
        tiles
          .flatMap(t => t.connectors.map(connector => connector -> t))
          .groupMap { case (connector, _) => connector } { case (_, tile) => tile }

      val tilesConnections: Map[TileId, Set[(TileId, Connector)]] =
        tiles.map { tile =>
          val tileConnections = for {
            connector <- tile.connectors
            connectedTiles = tilesByConnector(connector)
            connectedTile <- connectedTiles
            if connectedTile.id != tile.id
          } yield (connectedTile.id, connector)
          tile.id -> tileConnections
        }.toMap

      tilesConnections.map { case (k, l) => k -> l.toList.sortBy { case (tileId, connector) => tileId } }.foreach(println)
      println("---------------------------")

      val tilesUsedBorders =
        tiles.map { tile =>
          val otherTilesPossibleConnections: Map[TileId, List[Connector]] =
            tilesConnections
              .getOrElse(tile.id, Nil)
              .groupMap { case (otherTileId, connector) => otherTileId } { case (otherTileId, connector) => connector }
              .view
              .mapValues(_.toList)
              .toMap

          val candidates = possibleBordersFrom(tile.borders).filter { borders =>
            otherTilesPossibleConnections.forall { case (otherTileId, otherConnectors) =>
              otherConnectors.exists(connector => otherConnectors.contains(connector))
            }
          }

          println("******", tile.id, "******")
          println(candidates.size)
          //candidates.map(_.encoding).map(_.mkString("","\n","\n")).foreach(println)
          candidates.map(b => s"${b.up} ${b.down} ${b.left} ${b.right} => ${b.id}").foreach(println)
        }

      val tilesUsedConnectors =
        tilesConnections
          .map { case (tileId, connections) => tileId -> connections.map { case (id, cnx) => cnx } }

      val cornersTileId =
        tilesConnections
          .map { case (tileId, connections) => tileId -> connections.map { case (id, cnx) => id } }
          .collect { case (tileId, connectedTileIds) if connectedTileIds.size == 2 => tileId }

      println("************ corners **************")
      println(cornersTileId.mkString(" "))

      println("************ rebuild **************")
      var board = Map.empty[(Int, Int), Borders]
      var availableTileIds = tilesConnections.keys.toSet
      for {
        x <- 0 until size
        y <- 0 until size
        pos = (x, y)
      } {
        pos match {
          case (0, 0) =>
            for {
              tileId <- cornersTileId
              tile <- tilesById.get(tileId)
              inUseConnectors <- tilesUsedConnectors.get(tileId)
              borders <- possibleBordersFrom(tile.borders)
              if !board.contains(pos)
              if inUseConnectors.contains(borders.down) &&
                inUseConnectors.contains(borders.right)
            } {
              println(pos -> borders)
              availableTileIds -= tileId
              board += pos -> borders
            }
          case (0, _) =>
            for {
              tileId <- availableTileIds
              tile <- tilesById.get(tileId)
              upBorders <- board.get(x, y - 1)
              borders <- possibleBordersFrom(tile.borders)
              if borders.up == upBorders.down
            } {
              println(pos -> borders)
              availableTileIds -= tileId
              board += pos -> borders
            }
          case (_, 0) =>
            for {
              tileId <- availableTileIds
              tile <- tilesById.get(tileId)
              leftBorders <- board.get(x - 1, y)
              borders <- possibleBordersFrom(tile.borders)
              if borders.left == leftBorders.right
            } {
              println(pos -> borders)
              availableTileIds -= tileId
              board += pos -> borders
            }
          case (_, _) =>
            for {
              tileId <- availableTileIds
              tile <- tilesById.get(tileId)
              upBorders <- board.get(x, y - 1)
              leftBorders <- board.get(x - 1, y)
              borders <- possibleBordersFrom(tile.borders)
              if borders.left == leftBorders.right
              if borders.up == upBorders.down
            } {
              println(pos -> borders)
              availableTileIds -= tileId
              board += pos -> borders
            }
        }
      }

      val encoding =
        (0 until size).flatMap( y =>
          (0 until size)
            .map(x => board(x, y).encoding)
            .map(a => a.map(_.tail.init).tail.init)
            .reduceLeft[Array[String]] { case (a0, a1) => a0.zip(a1).map { case (l0, l1) => l0 + l1 } }
        )

      println(encoding.mkString("\n"))


    ???
  }

}

}

// =====================================================================================

class PuzzleDay20Test extends AnyFlatSpec with should.Matchers with Helpers {

  import PuzzleDay20._

  "basic functions" should "work" in {
    str2bin("10") shouldBe 2
    str2bin("100") shouldBe 4
    bin2str(4, 3) shouldBe "100"
    bin2str(4, 4) shouldBe "0100"
    bin2str(flipBits(str2bin("00101"), 5), 5) shouldBe "10100"
    bin2str(flipBits(str2bin("00001"), 5), 5) shouldBe "10000"
    bin2str(flipBits(str2bin("10000"), 5), 5) shouldBe "00001"
    bin2str(flipBits(str2bin("01000"), 5), 5) shouldBe "00010"
    bin2str(flipBits(str2bin("00100"), 5), 5) shouldBe "00100"
    flipBits(1, 3) shouldBe 4
    flipBits(4, 3) shouldBe 1
    //flipBits(bin2str)
  }

  "rotatingRight" should "encode in the right way" in {
    val from =
      """abc
        |def
        |ghj""".stripMargin.split("\n")
    rotateEncodingRight(from).mkString("\n") shouldBe
      """gda
        |heb
        |jfc""".stripMargin
  }

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

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay20.Part2._
    solve(resourceContent("day20/input-example-1.txt")) shouldBe 273
  }
  it should "give the right result on the input file" ignore {
    import PuzzleDay20.Part2._
    solve(resourceContent("day20/input-given-1.txt")) shouldBe -1
  }
}

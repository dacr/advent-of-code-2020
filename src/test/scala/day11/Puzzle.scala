package day11

// SOLVED 1h30m - next time take more time to fully read the puzzle text grrr... !!!

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.annotation.tailrec

object PuzzleDay11 {

  object Part1 {

    sealed trait Cell
    object Floor extends Cell {
      override def toString()="."
    }
    object Seat extends Cell{
      override def toString()="L"
    }
    object OccupiedSeat extends Cell {
      override def toString()="#"
    }


    val adjacents: Seq[(Int, Int)] = for {
      dx <- -1 to 1
      dy <- -1 to 1
      if !(dx==0 && dy == 0)
    } yield (dx,dy)

    def positions(w:Int,h:Int): Seq[(Int, Int)] = for {
      y <- 0 until h
      x <- 0 until w
    } yield (x,y)


    case class Board(area:Vector[Cell], width:Int, height:Int) {
      def occupiedSeats(): Long =
        area.count(_ == OccupiedSeat)

      override def toString():String =
        area.grouped(width).map(_.mkString).mkString("\n")


      def visibleOccupiedAdjacent(x:Int, y:Int):Int = {
        var count = 0
        adjacents.foreach { case (xv, yv) =>
          var xc = x+xv
          var yc = y+yv
          var continue = true
          while(xc>=0 && yc >=0 && xc < width && yc < height && continue) {
            if (isOccupied(xc,yc)) {
              count+=1
              continue=false
            }
            if (isFree(xc,yc)) continue = false
            xc += xv
            yc += yv
          }
        }
        count
      }

      def occupiedAdjacent(x:Int, y:Int):Int = {
        adjacents
          .map{case (dx,dy) => (x+dx, y+dy)}
          .filter{case (x,y) => x>=0 && y>=0 && x<width && y<height }
          .map(at)
          .count(_ == OccupiedSeat)
      }
      def at(x:Int,y:Int):Cell = area(y*width+x)
      def at(pos:(Int,Int)):Cell = pos match {case (x,y) =>area(y*width+x)}
      def isFree(x:Int, y:Int):Boolean = at(x,y) == Seat
      def isOccupied(x:Int, y:Int):Boolean = at(x,y) == OccupiedSeat
      def leave(x:Int,y:Int):Board = Board(area.updated(y*width+x, Seat),width,height)
      def take(x:Int,y:Int):Board = Board(area.updated(y*width+x, OccupiedSeat),width,height)
    }

    def char2cell(in:Char):Cell = in match {
      case '.' => Floor
      case 'L' => Seat
      case '#' => OccupiedSeat
    }

    def playAll(original:Board):Board = {
      var newBoard = original.copy()
      positions(original.width, original.height).foreach{case (x,y) =>
        if (original.isFree(x,y) && original.occupiedAdjacent(x,y)==0) newBoard = newBoard.take(x,y)
        if (original.isOccupied(x,y) && original.occupiedAdjacent(x,y)>=4) newBoard = newBoard.leave(x,y)
      }
      newBoard
    }

    def solve(input: Iterable[String]): Long = {
      val area = input.map(_.map(char2cell).toArray).toVector
      val (w, h) = (area.head.size, area.size)
      LazyList
        .iterate(Board(area.flatten,w,h))(playAll)
        .sliding(2)
        .collect{case LazyList(a,b) if a==b => a.occupiedSeats()}
        .next()
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {
    import Part1._

    def playAll2(original:Board):Board = {
      var newBoard = original.copy()
      positions(original.width, original.height).foreach { case (x,y) =>
        if (original.isFree(x,y) && original.visibleOccupiedAdjacent(x,y)==0) newBoard = newBoard.take(x,y)
        if (original.isOccupied(x,y) && original.visibleOccupiedAdjacent(x,y)>=5) newBoard = newBoard.leave(x,y)
      }
      newBoard
    }

    def solve2(input: Iterable[String]): Long = {
      val area = input.map(_.map(char2cell)).toVector
      val (w, h) = (area.head.size, area.size)
      LazyList
        .iterate(Board(area.flatten,w,h))(playAll2)
        .sliding(2)
        .collect{case LazyList(a,b) if a==b => a.occupiedSeats() }
        .next()
    }
  }

}

// =====================================================================================

class PuzzleDay11Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay11.Part1._
    solve(resourceContentLines("day11/input-example-1.txt")) shouldBe 37
  }
  it should "give the right result on the input file" in {
    import PuzzleDay11.Part1._
    solve(resourceContentLines("day11/input-given-1.txt")) shouldBe 2166
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay11.Part2._
    solve2(resourceContentLines("day11/input-example-1.txt")) shouldBe 26
  }
  it should "give the right result on the input file" in {
    import PuzzleDay11.Part2._
    solve2(resourceContentLines("day11/input-given-1.txt")) shouldBe 1955
  }

}

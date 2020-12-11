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


    case class Board(area:Vector[Cell], w:Int, h:Int) {
      def occupiedSeats(): Long =
        area.count(_ == OccupiedSeat)

      override def toString():String =
        area.grouped(w).map(_.mkString).mkString("\n")


      def visibleOccupiedAdjacent(x:Int, y:Int):Int = {
        var count = 0
        adjacents.foreach { case (xv, yv) =>
          var xc = x+xv
          var yc = y+yv
          var continue = true
          while(xc>=0 && yc >=0 && xc < w && yc < h && continue) {
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
          .filter{case (x,y) => x>=0 && y>=0 && x<w && y<h }
          .map(at)
          .count(_ == OccupiedSeat)
      }
      def at(x:Int,y:Int):Cell = area(y*w+x)
      def at(pos:(Int,Int)):Cell = pos match {case (x,y) =>area(y*w+x)}
      def isFree(x:Int, y:Int):Boolean = at(x,y) == Seat
      def isOccupied(x:Int, y:Int):Boolean = at(x,y) == OccupiedSeat
      def leave(x:Int,y:Int):Board = Board(area.updated(y*w+x, Seat),w,h)
      def take(x:Int,y:Int):Board = Board(area.updated(y*w+x, OccupiedSeat),w,h)
    }

    def char2cell(in:Char):Cell = in match {
      case '.' => Floor
      case 'L' => Seat
      case '#' => OccupiedSeat
    }

    def playAll(original:Board):Board = {
      var newBoard = original.copy()
      positions(newBoard.w, newBoard.h).foreach{case (x,y) =>
        if (original.isFree(x,y) && original.occupiedAdjacent(x,y)==0) newBoard = newBoard.take(x,y)
        if (original.isOccupied(x,y) && original.occupiedAdjacent(x,y)>=4) newBoard = newBoard.leave(x,y)
      }
      newBoard
    }

    def solve(input: Iterable[String]): Long = {
      val area = input.map(_.map(char2cell).toArray).toVector
      val w = area.head.size
      val h = area.size
      var board = Board(area.flatten,w,h)
      var continue = true
      do {
        val newBoard = playAll(board)
        //println(newBoard+"\n")
        if (newBoard == board) continue = false
        board = newBoard
      } while(continue)
      board.occupiedSeats()
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {
    import Part1._

    def playAll2(original:Board):Board = {
      var newBoard = original.copy()
      for {
        y <- 0 until original.h
        x <- 0 until original.w
      } {
        if (original.isFree(x,y) && original.visibleOccupiedAdjacent(x,y)==0) newBoard = newBoard.take(x,y)
        if (original.isOccupied(x,y) && original.visibleOccupiedAdjacent(x,y)>=5) newBoard = newBoard.leave(x,y)
      }
      newBoard
    }

    def solve2(input: Iterable[String]): Long = {
      val area = input.map(_.map(char2cell)).toVector
      val w = area.head.size
      val h = area.size
      var board = Board(area.flatten,w,h)
      var continue = true
      do {
        val newBoard = playAll2(board)
        if (newBoard == board) continue = false
        board = newBoard
      } while(continue)
      board.occupiedSeats()
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

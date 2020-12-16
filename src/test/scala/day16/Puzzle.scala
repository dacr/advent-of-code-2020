package day16

// solved in ~1h15m

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.math._

import scala.annotation.tailrec

object PuzzleDay16 {

  case class Constraint(spec:String) {
    val parts = spec.split(":\\s*",2)
    val name = parts(0)
    val ranges =
      parts(1)
        .split(" or ")
        .map(_.trim)
        .filter(_.size>0)
        .map(_.split("-",2).map(_.toInt))
        .map { case Array(a,b) => a->b}

    def isIn(v:Int):Boolean = ranges.exists{case (a,b) => a<=v && v <=b}
  }

  object Part1 {

    def solve(input: String): Long = {
      val parts = input.split("\n\n")
      val constraints = parts.head.split("\n").map(_.trim).filter(_.size>0).map(Constraint).toList
      val yourTicket = parts.tail.head.split("\n").tail.head.split(",").map(_.toInt).toList
      val nearbyTickets = parts.drop(2).head.split("\n").tail.map(_.split(",").map(_.toInt).toList).toList

      nearbyTickets.flatMap{ values =>
        values.filterNot(v => constraints.exists(_.isIn(v)))
      }.sum
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {

    def solve(input: String): Long = {
      val parts = input.split("\n\n")
      val constraints = parts.head.split("\n").map(_.trim).filter(_.size>0).map(Constraint).toList
      val yourTicket = parts.tail.head.split("\n").tail.head.split(",").map(_.toInt).toArray
      val nearbyTickets = parts.drop(2).head.split("\n").tail.map(_.split(",").map(_.toInt).toList).toList

      val validTickets = nearbyTickets.filter{ values =>
        values.forall(v => constraints.exists(_.isIn(v)))
      }

      var groupedByIndex=
        validTickets
          .flatMap(_.zipWithIndex).groupMap {case (v,i) => i } {case (v,i) => v}


      var fieldIdents = Map.empty[Constraint,Int]

      var remainingConstraints = constraints.toSet
      while(remainingConstraints.size >0) {
        println(remainingConstraints)
        remainingConstraints.map{case c =>
            c -> groupedByIndex.collect{case (i,values) if values.forall(v => c.isIn(v) ) => i}
          }.find{case (c,indexes) => indexes.size == 1} match {
            case Some((c, i)) =>
              remainingConstraints -= c
              fieldIdents += c->i.head
              groupedByIndex = groupedByIndex.removed(i.head)
            case None => assert(false)
          }
      }

      fieldIdents.collect{case (c, i) if c.name.startsWith("departure") => yourTicket(i).toLong}.product

    }

  }

}

// =====================================================================================

class PuzzleDay16Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay16.Part1._
    solve(resourceContent("day16/input-example-1.txt")) shouldBe 71
  }
  it should "give the right result on the input file" in {
    import PuzzleDay16.Part1._
    solve(resourceContent("day16/input-given-1.txt")) shouldBe 19240
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" ignore {
    import PuzzleDay16.Part2._
    solve(resourceContent("day16/input-example-1.txt")) shouldBe -1
  }
  it should "give the right result on the input file" in {
    import PuzzleDay16.Part2._
    solve(resourceContent("day16/input-given-1.txt")) should not be 36581
    solve(resourceContent("day16/input-given-1.txt")) shouldBe 21095351239483L
  }

}

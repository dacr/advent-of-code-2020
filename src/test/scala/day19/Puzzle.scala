package day19

// solved in XXmn

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.util.regex.Pattern
import scala.math._
import scala.annotation.tailrec
import scala.util.matching.Regex

object PuzzleDay19 {
  type Messages=List[String]

  object Segment{
    def intsOf(in:String):List[Int] = {
      in.split(" ").map(_.toInt).toList
    }
    def apply(in:String):Segment = {
      in match {
        case s""""$a"""" => SegmentMatch(a)
        case s"$left | $right" => SegmentOr(intsOf(left), intsOf(right))
        case parts =>  SegmentAnd(intsOf(parts))
      }
    }
  }
  sealed trait Segment
  case class SegmentMatch(in:String) extends Segment
  case class SegmentOr(left:List[Int], right:List[Int]) extends Segment
  case class SegmentAnd(parts:List[Int]) extends Segment

  def toPattern(pos:Int, rules:Map[Int,Segment]):String = {
    rules(pos) match {
      case SegmentMatch(a) => a
      case SegmentAnd(parts) => parts.map(pos => toPattern(pos, rules)).mkString
      case SegmentOr(left,right) if right.last == pos =>
        val l = left.map(pos => toPattern(pos, rules)).mkString
        val r = right.filterNot(_ == pos).map(pos => toPattern(pos, rules)).mkString
        s"(($l)+)"
      case SegmentOr(left,right) if right.contains(pos) =>
        val (tl,tr) = right.splitAt(right.indexOf(pos))
        val ntl = tl.map(pos => toPattern(pos, rules)).mkString
        val ntr = tr.tail.map(pos => toPattern(pos, rules)).mkString
        //s"(($ntl)+($ntr)+)" // TODO - IN Fact not the right approach as both must have the same abount of occurences !
        (1 to 5).map(i => s"(($ntl){$i}($ntr){$i})").mkString("(","|",")") // TODO VERY BAD HACK !!!
      case SegmentOr(left,right) =>
        val l = left.map(pos => toPattern(pos, rules)).mkString
        val r = right.map(pos => toPattern(pos, rules)).mkString
        s"(($l)|($r))"
    }
  }

  def decode(input:String):(Regex,Messages) = {
    val Array(specs, msgs) = input.trim.split("\n\n", 2)
    val rules =
      specs
        .split("\n")
        .map(_.split(": ",2))
        .map{case Array(k, v) => k.trim.toInt -> Segment(v)}
        .toMap

    val regex = "^"+toPattern(0,rules)+"$"
    (regex.r,msgs.split("\n").to(List))
  }

  object Part1 {


    def solve(input: String): Long = {
      val (regex, messages) = decode(input)
      messages.count(regex.matches)
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {

    def solve(input: String): Long = {
      val (regex, messages) = decode(input)
      messages.count(regex.matches)
    }
  }

}

// =====================================================================================

class PuzzleDay19Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay19.Part1._
    solve(resourceContent("day19/input-example-1.txt")) shouldBe 2
  }
  it should "give the right result on the input file" in {
    import PuzzleDay19.Part1._
    solve(resourceContent("day19/input-given-1.txt")) shouldBe 132
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay19.Part2._
    val content =
      resourceContent("day19/input-example-2.txt")
        .replaceAll("(?m)^8: 42$", "8: 42 | 42 8")
        .replaceAll("(?m)^11: 42 31$", "11: 42 31 | 42 11 31")
    solve(content) shouldBe 12
  }
  it should "give the right result on the input file" in {
    import PuzzleDay19.Part2._
    val content =
      resourceContent("day19/input-given-1.txt")
        .replaceAll("(?m)^8: 42$", "8: 42 | 42 8")
        .replaceAll("(?m)^11: 42 31$", "11: 42 31 | 42 11 31")

    solve(content) should not be 236
    solve(content) should not be 227
    solve(content) should not be 313
    solve(content) shouldBe 306
  }
}

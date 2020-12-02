package day2

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

object Puzzle {

  def checkPolicy(policy:String, password:String): Boolean = {
    val Array(range, chars) = policy.split(" ", 2)
    val Array(min,max) = range.split("-", 2).map(_.toInt)
    val ch=chars.head
    val count = password.count(_==ch)
    count >= min && count <= max
  }

  def solve1(input:Iterable[String]):Int = {
    input
      .map(_.split(":",2).map(_.trim))
      .filter{case Array(policy, password) => checkPolicy(policy, password)}
      .size
  }

  // -------------------------------------------------------------------------

  def checkPolicy2(policy:String, password:String): Boolean = {
    val Array(range, chars) = policy.split(" ", 2)
    val Array(pos1,pos2) = range.split("-", 2).map(_.toInt)
    val ch=chars.head
    (password(pos1-1) == ch || password(pos2-1) == ch) && password(pos1-1) != password(pos2-1)
  }


  def solve2(input:Iterable[String]):Int = {
    input
      .map(_.split(":",2).map(_.trim))
      .filter{case Array(policy, password) => checkPolicy2(policy, password)}
      .size
  }

}

class PuzzleTest extends AnyFlatSpec with should.Matchers with Helpers {
  import Puzzle._

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    solve1(resourceContent("day2/input-example-1.txt")) shouldBe 2
  }

  it should "give the right answer on the given file" in {
    solve1(resourceContent("day2/input-given-1.txt")) shouldBe 603
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    solve2(resourceContent("day2/input-example-1.txt")) shouldBe 1
  }

  it should "give the right answer on the given file" in {
    solve2(resourceContent("day2/input-given-1.txt")) shouldBe 404
  }

}

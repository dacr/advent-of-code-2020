package day18

// solved in XXmn

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.math._

import scala.annotation.tailrec

object PuzzleDay18 {

  sealed trait Token

  case class Num(v: Long) extends Token {
    override def toString: String = v.toString
  }

  trait Op extends Token {
    def op(a: Long, b: Long): Long
  }

  object AddOp extends Op {
    def op(a: Long, b: Long): Long = a + b

    override def toString: String = "+"
  }

  object MulOp extends Op {
    def op(a: Long, b: Long): Long = a * b

    override def toString: String = "*"
  }

  object ParOpen extends Token {
    override def toString: String = "("
  }

  object ParClose extends Token {
    override def toString: String = ")"
  }

  type Expr = List[Token]

  val NumToken = """(\d+)(.*)""".r
  val AddToken = """[+](.*)""".r
  val MulToken = """[*](.*)""".r
  val OpenToken = """[(](.*)""".r
  val CloseToken = """[)](.*)""".r

  def tokenize(input: String, accu: Expr = Nil): Expr = {
    input match {
      case "" => accu
      case NumToken(vs, remain) => tokenize(remain, accu :+ Num(vs.toLong))
      case AddToken(remain) => tokenize(remain, accu :+ AddOp)
      case MulToken(remain) => tokenize(remain, accu :+ MulOp)
      case OpenToken(remain) => tokenize(remain, accu :+ ParOpen)
      case CloseToken(remain) => tokenize(remain, accu :+ ParClose)
    }
  }


  object Part1 {

    def decompose(expr: Expr): (Expr, Expr) = {
      var parCount = 1
      var closingParIndex = 0
      var remainExpr = expr
      while (parCount > 0 && remainExpr.nonEmpty) {
        remainExpr.head match {
          case ParOpen => parCount += 1
          case ParClose => parCount -= 1
          case _ =>
        }
        remainExpr = remainExpr.tail
        closingParIndex += 1
      }
      (expr.take(closingParIndex - 1), remainExpr)
    }

    def evaluate(expr: Expr): Option[Long] = {
      //println("---------- " ,expr.mkString)
      def worker(expr: Expr, accu: Option[Long]): Option[Long] = {
        //println(accu, " => " ,expr.mkString)
        expr match {
          case Nil => accu
          case Num(v) :: remain => worker(remain, Some(v))
          case ParOpen :: remain =>
            val (left, right) = decompose(remain)
            worker(right, worker(left, None))
          case AddOp :: Num(v) :: remain => worker(remain, accu.map(_ + v))
          case MulOp :: Num(v) :: remain => worker(remain, accu.map(_ * v))
          case AddOp :: ParOpen :: remain =>
            val (left, right) = decompose(remain)
            worker(right, worker(left, None).map(_ + accu.getOrElse(0L)))
          case MulOp :: ParOpen :: remain =>
            val (left, right) = decompose(remain)
            worker(right, worker(left, None).map(_ * accu.getOrElse(1L)))
        }
      }

      worker(expr, None)
    }

    def solve(input: String): Long = {
      input.split("\n").map(_.replaceAll("\\s+", "")).map(v => tokenize(v)).flatMap(evaluate).sum
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {

    def evaluate(expr: Expr): Long = {
      def reduceParAdds(expr: Expr): Expr = {
        expr match {
          case Nil => Nil
          case ParOpen :: Num(a) :: AddOp :: Num(b) :: ParClose :: remain => Num(a + b) ::remain
          case ParOpen :: Num(v) :: ParClose :: remain => Num(v) ::remain
          case head :: tail => head :: reduceParAdds(tail)
        }
      }

      def reduceParMuls(expr: Expr): Expr = {
        expr match {
          case Nil => Nil
          case ParOpen :: Num(a) :: MulOp :: Num(b) :: ParClose :: remain => Num(a * b) :: remain
          case ParOpen :: Num(v) :: ParClose :: remain => Num(v) :: remain
          case head :: tail => head :: reduceParMuls(tail)
        }
      }

      def reduceAdds(expr: Expr): Expr = {
        expr match {
          case Nil => Nil
          case Num(a) :: AddOp :: Num(b) :: remain => Num(a + b) :: remain
          case ParOpen :: Num(v) :: ParClose :: remain => Num(v) :: remain
          case head :: tail => head :: reduceAdds(tail)
        }
      }

      def reduceMuls(expr: Expr): Expr = {
        expr match {
          case Nil => Nil
          case Num(a) :: MulOp :: Num(b) :: remain => Num(a * b) :: remain
          case ParOpen :: Num(v) :: ParClose :: remain => Num(v) :: remain
          case head :: tail => head :: reduceMuls(tail)
        }
      }

      def reduce(expr: Expr): Long = {
        reduceParMuls(expr) match {
          case newExpr if newExpr != expr => reduce(newExpr)
          case Num(v) :: Nil => v
          case otherwise =>
            reduceParAdds(expr) match {
              case newExpr if newExpr != expr => reduce(newExpr)
              case Num(v) :: Nil => v
              case otherwise =>
                reduceAdds(expr) match {
                  case newExpr if newExpr != expr => reduce(newExpr)
                  case Num(v) :: Nil => v
                  case otherwise =>
                    reduceMuls(expr) match {
                      case newExpr if newExpr != expr => reduce(newExpr)
                      case Num(v) :: Nil => v
                    }
                }
            }
        }
      }

      reduce(expr)
    }

    def solve(input: String): Long = {
      input.split("\n").map(_.replaceAll("\\s+", "")).map(v => tokenize(v)).map(evaluate).sum
    }
    def solve2(input: String): Long = {
      val lines = input.split("\n").map(_.replaceAll("\\s+", ""))
      evaluate(tokenize(lines.mkString("+")))
    }
  }

}

// =====================================================================================

class PuzzleDay18Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay18.Part1._
    solve(resourceContent("day18/input-example-1.txt")) shouldBe 71
  }
  it should "give the right results on the other examples" in {
    import PuzzleDay18.Part1._
    solve("2 * 3 + (4 * 5)") shouldBe 26
    solve("5 + (8 * 3 + 9 + 3 * 4 * 3)") shouldBe 437
    solve("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") shouldBe 12240
    solve("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") shouldBe 13632
  }
  it should "give the right result on the input file" in {
    import PuzzleDay18.Part1._
    solve(resourceContent("day18/input-given-1.txt")) shouldBe 3348222486398L
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay18.Part2._
    solve(resourceContent("day18/input-example-1.txt")) shouldBe 231
  }
  it should "give the right results on the other examples" in {
    import PuzzleDay18.Part2._
    solve("1 + (2 * 3) + (4 * (5 + 6))") shouldBe 51
    solve("2 * 3 + (4 * 5)") shouldBe 46
    solve("5 + (8 * 3 + 9 + 3 * 4 * 3)") shouldBe 1445
    solve("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") shouldBe 669060
    solve("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") shouldBe 23340
  }
  it should "work with more tests" in {
    import PuzzleDay18.Part2._
    solve("3") shouldBe 3
    solve("(3)") shouldBe 3
    solve("((3))") shouldBe 3

    solve("3+2") shouldBe 5
    solve("(3+2)") shouldBe 5
    solve("((3+2))") shouldBe 5

    solve("3*2") shouldBe 6
    solve("(3*2)") shouldBe 6
    solve("((3*2))") shouldBe 6


    solve("2 * (5 + 7)") shouldBe 24
    solve("(2 * 5 ) + 7") shouldBe 17
    solve("3 + (2 * 5 ) + 7") shouldBe 20

    solve("2 + (5 * 7)") shouldBe 37
    solve("(2 + 5 ) * 7") shouldBe 49
    solve("3 * (2 + 5) * 7") shouldBe 21*7

    solve("3 * (2 + (2 + 3)) * 7") shouldBe 21*7
    solve("3 * (2 + (2 * 3)) * 7") shouldBe 24*7
    solve("(3) * (2 + (2 * 3)) * 7") shouldBe 24*7
    solve("(3 * (2 + (2 * 3)) * 7)") shouldBe 24*7

    solve("(2 + ( 2 + ( 2 + (2 * 3))))") shouldBe 12
    solve("((((2 * 3) + 2) + 2) + 2)") shouldBe 12

    solve("(2 * ( 2 * ( 2 * (2 + 3))))") shouldBe 40
    solve("((((2 + 3) * 2) * 2) * 2)") shouldBe 40

    solve("(2 * ( 2 * ( 2 * (2 + 3)))) + 1 + (1) + ((((2 + 3) * 2) * 2) * 2)") shouldBe 82

    solve("(2+3)\n(3*2)") shouldBe 11

    solve("2 + 2 * 2 + 2 * 2") shouldBe 32
    solve("2 + (2 * 2) + (2 * 2)") shouldBe 10

    solve("10 + 10") shouldBe 20
    solve("10 + 10 * 20") shouldBe 400

    solve("9223372036854775807") shouldBe 9223372036854775807L
  }
  it should "work on basic example#1" in {
    import PuzzleDay18.Part2._
    solve("2 * 2 + 2 * 2 + 2 * 2 + 2") shouldBe 128
    solve("2 * 2 + 2 * 2 + 2 * 2 + 2") shouldBe solve("2 * (2 + 2) * (2 + 2) * (2 + 2)")
  }
  it should "work on basic example#2" in {
    import PuzzleDay18.Part2._
    solve("2 + 2 * 2 + 2 * 2 + 2 * 2 + 3") shouldBe solve("(2 + 2) * (2 + 2) * (2 + 2) * (2 + 3)")
    solve("2 + 2 * 2 + 2 * 2 + 2 * 2 + 3") shouldBe 320
  }
  it should "work on basic example#3" in {
    import PuzzleDay18.Part2._
    solve("2 + (2 * 2 + 2) * 2 + 2 * 2 + 3") shouldBe solve("(2 + (2 * (2 + 2))) * (2 + 2) * (2 + 3)")
  }
  it should "give the right result on the input file" in {
    import PuzzleDay18.Part2._
    solve(resourceContent("day18/input-given-1.txt")) should not be 27132530028608L
    solve(resourceContent("day18/input-given-1.txt")) should not be 27132530028608L + 3348222486398L
    solve(resourceContent("day18/input-given-1.txt")) should not be 27132530028608L * 2
    // A lot of time lost because because in progress reduce should be done up to the end, do not wait for next iteration...
    solve(resourceContent("day18/input-given-1.txt")) should not be 28055119131161L
    solve(resourceContent("day18/input-given-1.txt")) shouldBe 28055119131161L
  }
  it should "give the addup solutions" in {
    import PuzzleDay18.Part2._
    solve2(resourceContent("day18/input-given-1.txt")) shouldBe 1L
  }

}

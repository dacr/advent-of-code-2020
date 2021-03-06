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

  object AddOp extends Token {
    def op(a: Long, b: Long): Long = a + b

    override def toString: String = "+"
  }

  object MulOp extends Token {
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

  @tailrec
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


  object Part1 {

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

    def reducePar(expr: Expr): Expr = expr match {
      case Nil => Nil
      case ParOpen :: Num(v) :: ParClose :: remain => Num(v) :: remain
      case ParOpen :: remain =>
        val (left, right) = decompose(remain)
        reduce(left):::right
      case head :: tail => head :: reducePar(tail)
    }

    def reduceAdds(expr: Expr): Expr = expr match {
      case Nil => Nil
      case Num(a) :: AddOp :: Num(b) :: remain => Num(a + b) :: remain
      case head :: tail => head :: reduceAdds(tail)
    }

    def reduceMuls(expr: Expr): Expr = expr match {
      case Nil => Nil
      case Num(a) :: MulOp :: Num(b) :: remain => Num(a * b) :: remain
      case head :: tail => head :: reduceMuls(tail)
    }

    @tailrec
    def reduce(expr: Expr): Expr = {
      //println(expr.mkString)
      reducePar(expr) match {
        case newExpr if newExpr != expr => reduce(newExpr)
        case _ =>
          reduceAdds(expr) match {
            case newExpr if newExpr != expr => reduce(newExpr)
            case _ =>
              reduceMuls(expr) match {
                case newExpr if newExpr != expr => reduce(newExpr)
                case v => v
              }
          }
      }
    }


    def evaluate(expr: Expr): Long = {
      reduce(expr) match {
        case Num(v) :: Nil => v
      }
    }

    def solve(input: String): Long = {
      val exprs =
        input
          .split("\n")
          .map(_.replaceAll("\\s+", ""))
          .map(v => tokenize(v))
      val results = exprs.map(evaluate)
      //println(exprs.zip(results).sortBy{case (_,result)=>result}.map{case (expr,result)=> ""+result+" = "+expr.mkString}.mkString("\n"))
      results.sum
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
    solve("3 * (2 + 5) * 7") shouldBe 21 * 7

    solve("2 + 3 * 4 + 5") shouldBe solve("(2 + 3) * (4 + 5)")
    solve("2 * 3 + 4 * 5") shouldBe solve("2 * (3 + 4) * 5")

    solve("3 * (2 + (2 + 3)) * 7") shouldBe 21 * 7
    solve("3 * (2 + (2 * 3)) * 7") shouldBe 24 * 7
    solve("(3) * (2 + (2 * 3)) * 7") shouldBe 24 * 7
    solve("(3 * (2 + (2 * 3)) * 7)") shouldBe 24 * 7

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
  it should "work on basic example#4" in {
    import PuzzleDay18.Part2._
    solve("(1 + 2 + 3) * ((2 * 3 + 2) + 1)") shouldBe 66
  }
  it should "work on basic example#5" in {
    import PuzzleDay18.Part2._
    solve("1 + 1 + 1 + 1 + 1 * 2 * 2 * 2 * 2 * 2") shouldBe 160
  }
  it should "work on basic example#6" in {
    import PuzzleDay18.Part2._
    solve("1 * 1 * 1 * 1 * 1 + 2 + 2 + 2 + 2 + 2") shouldBe 11
  }
  it should "work on basic example#7" in {
    import PuzzleDay18.Part2._
    solve("( 3 * 3 ) + (2 + 2 + 2 + 2 + 2)") shouldBe 19
  }
  it should "work on basic example#8" in {
    import PuzzleDay18.Part2._
    solve("( 3 + 3 ) * (2 + 2)") shouldBe 24
  }
  it should "work on basic example#9" in {
    import PuzzleDay18.Part2._
    solve("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") shouldBe 23340
    solve("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") shouldBe
      solve("((((2 + 4) * 9) * ((6 + 9) * (8 + 6)) + 6) + (2 + 4)) * 2")
  }
  it should "work on basic example#10" in {
    import PuzzleDay18.Part2._
    solve("(((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2)+(((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2)") shouldBe 23340 * 2
    solve("(((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2)*(((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2)") shouldBe 23340 * 23340
  }
  it should "work on basic example#11" in {
    import PuzzleDay18.Part2._
    solve("3 + (4 + 8 + 3 + 4 + 7 + 6) + 4 + 3 * 4 + ((5 * 6) + 2 * 5 * 2 + 8 * 3)") shouldBe 201768
  }
  it should "work on basic example#12" in {
    import PuzzleDay18.Part2._
    solve("6*(2+4*8*7*4*(7*4*9*3+9))*(6*7+7*6*8+5)*3*8*(7+2*2*4)") shouldBe
      solve("6*((2+4)*8*7*4*(7*4*9*(3+9)))*(6*(7+7)*6*(8+5))*3*8*((7+2)*2*4)")
  }
  it should "work on basic example#13" in {
    import PuzzleDay18.Part2._
    solve("1+2\n1+2\n2+(3*4)") shouldBe 20
    solve("1+2\n1+2\n2+(3*4)\n") shouldBe 20
    solve("1+2\n1+2\n2+(3*4)\n") shouldBe 20
  }
  it should "work on basic example#14" in {
    import PuzzleDay18.Part2._
    info("first prioritize parenthesis evaluation")
    solve("3*1604460*2+(27*4*7*9*48)+36") shouldBe (((27L * 4 * 7 * 9 * 48) + 2) + 36) * 3 * 1604460
  }

  it should "give the right result on the input file" in {
    import PuzzleDay18.Part2._
    solve(resourceContent("day18/input-given-1.txt")) should not be 27132530028608L
    solve(resourceContent("day18/input-given-1.txt")) should not be 27132530028608L + 3348222486398L
    solve(resourceContent("day18/input-given-1.txt")) should not be 27132530028608L * 2
    solve(resourceContent("day18/input-given-1.txt")) should not be 28055119131161L
    solve(resourceContent("day18/input-given-1.txt")) shouldBe 43423343619505L
  }
  it should "give the right result on an other input file#2" in {
    import PuzzleDay18.Part2._
    solve(resourceContent("day18/input-given-2.txt")) shouldBe 297139939002972L
  }
  it should "give the right result on an other input file#3" in {
    import PuzzleDay18.Part2._
    solve(resourceContent("day18/input-given-3.txt")) shouldBe 20394514442037L
  }
}

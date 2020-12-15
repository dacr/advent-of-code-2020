package day14

// J'adore l'exemple pour nous induire en erreur et nous faire croire qu'on a du binaire si on lit trop vite l'énoncé !!!

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.math._

import scala.annotation.tailrec

object PuzzleDay14 {

  object Part1 {

    def binToLong(in:String):Long = java.lang.Long.parseLong(in,2)
    def longToBin(in:Long):String = in.toBinaryString

    def solve(input: String): Long = {
      val instructRE="""mem\[(\d+)\] = (\d+)""".r
      var mem = Map.empty[Int,Long]
      for {
        rawProgram <- input.split("mask = ").map(_.trim).filter(_.size > 0)
        program = rawProgram.split("\n")
        rawMask = program.head
        ignoreMask = binToLong(rawMask.replaceAll("[10]","0").replaceAll("X","1"))
        orMask = binToLong(rawMask.replaceAll("X", "0"))
        delMask = binToLong(rawMask.replaceAll("1", "X").replaceAll("0", "1").replaceAll("X","0"))
        (address,value) <- program.tail.collect{case instructRE(addr, valueString) => addr.toInt -> valueString.toLong}
      } {
        val newValue = (value | orMask) & ~delMask
        mem += address -> newValue
      }
      mem.values.sum
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {

    def binToLong(in:String):Long = java.lang.Long.parseLong(in,2)
    def longToStr(in:Long):String = in.toBinaryString

    def solve(input: String): Long = {
      val instructRE="""mem\[(\d+)\] = (\d+)""".r
      var mem = Map.empty[Long,Long]
      for {
        rawProgram <- input.split("mask = ").map(_.trim).filter(_.size > 0)
        program = rawProgram.split("\n")
        rawMask = program.head
        floatingPositionCount = rawMask.count(_ == 'X')
        floatingPositionIndexes = rawMask.toCharArray.reverse.zipWithIndex.collect{case ('X', i) => i}
        mask = binToLong(rawMask.replaceAll("[10]","0").replaceAll("X","1"))
        orMask = binToLong(rawMask.replaceAll("X", "0"))
        (address,value) <- program.tail.collect{case instructRE(addr, valueString) => addr.toInt -> valueString.toLong}
        baseAddress = (address | orMask) & ~mask
        floating <- 0 to pow(2,floatingPositionCount).toInt
        floatingAddress = longToStr(floating).reverse.zip(floatingPositionIndexes).collect{case ('1',i)=> pow(2,i).toLong}.sum
      } {
        mem += (baseAddress | floatingAddress) -> value
      }
      mem.values.sum
    }

  }

}

// =====================================================================================

class PuzzleDay14Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay14.Part1._
    solve(resourceContent("day14/input-example-1.txt")) shouldBe 165
  }
  it should "give the right result on the input file" in {
    import PuzzleDay14.Part1._
    solve(resourceContent("day14/input-given-1.txt")) should not be 41482740171L
    solve(resourceContent("day14/input-given-1.txt")) shouldBe 16003257187056L
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay14.Part2._
    solve(resourceContent("day14/input-example-2.txt")) shouldBe 208
  }
  it should "give the right result on the input file" in {
    import PuzzleDay14.Part2._
    solve(resourceContent("day14/input-given-1.txt")) should not be 16003257187056L
    solve(resourceContent("day14/input-given-1.txt")) shouldBe 3219837697833L
  }

}

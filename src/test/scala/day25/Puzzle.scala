package day25

// solved in XXmn

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.math._
import scala.annotation.tailrec
import scala.collection.immutable.Queue


object PuzzleDay25 {

  type SubjectNumber=Long
  type LoopSize=Int

  def handshake(subjectNumber:SubjectNumber, loopSize:LoopSize):Long = {
    @tailrec
    def loop(value:Long, remainLoop:LoopSize):Long = {
      if (remainLoop==0L) value
      else loop((value * subjectNumber) % 20201227L, remainLoop - 1)
    }
    loop(1L, loopSize)
  }

  def searchLoopSize(subjectNumber:SubjectNumber):LoopSize = {
    // -----------------------
    //Iterator
    //  .iterate(1)(_ + 1)
    //  .find(loopSize => handshake(7L,loopSize) == subjectNumber)
    //  .head
    // -----------------------
    //var loopSize=2
    //while(handshake(7L,loopSize)!=subjectNumber) loopSize+=1
    //loopSize
    // -----------------------
    @tailrec
    def loop(value:Long, currentLoopSize:LoopSize):LoopSize = {
      if (value==subjectNumber) currentLoopSize
      else loop((value * 7L) % 20201227L, currentLoopSize + 1)
    }
    loop(1L, 0)
  }



  object Part1 {

    def solve(cardPublicKey:Long, doorPublicKey:Long): Long = {
      //val cardLoopSize = searchLoopSize(cardPublicKey)
      //handshake(doorPublicKey, cardLoopSize)
      val doorLoopSize = searchLoopSize(doorPublicKey)
      handshake(cardPublicKey, doorLoopSize)
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

class PuzzleDay25Test extends AnyFlatSpec with should.Matchers with Helpers {

  import PuzzleDay25._
  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay25.Part1._
    handshake(7,8) shouldBe 5764801L
    handshake(7,11) shouldBe 17807724L
    handshake(5764801,11) shouldBe 14897079L
    handshake(17807724,8) shouldBe 14897079L
    solve(5764801L, 17807724L) shouldBe 14897079L
  }
  it should "give the right result on the input file" in {
    import PuzzleDay25.Part1._
    solve(10604480L, 4126658L) shouldBe 4968512L
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" ignore {
    import PuzzleDay25.Part2._
    solve(resourceContent("day25/input-example-1.txt")) shouldBe -1
  }
  it should "give the right result on the input file" ignore {
    import PuzzleDay25.Part2._
    solve(resourceContent("day25/input-given-1.txt")) shouldBe -1
  }
}

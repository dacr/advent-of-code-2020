package day7
// SOLVED IN ~2h !!

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.annotation.tailrec

object PuzzleDay7 {

  case class Bag(variant:String, color:String)
  case class Rule(lhs:Bag, rhs:Map[Bag,Int])

  object Part1 {

    def makeBagCount(input:String):(Bag,Int) = input.trim.split(" ") match {
      case Array(n, v, c) => (Bag(v, c), n.toInt)
    }
    def makeBag(input:String):Bag = input.trim.split(" ") match {
      case Array(v,c) => Bag(v,c)
    }
    def parseRules(input:String) = {
      input.split("\n").map{line =>
        line.split(" bags? contain ", 2) match {
          case Array(a,b) if b == "no other bags." => Rule(makeBag(a), Map.empty)
          case Array(a,b) =>
            val bag = makeBag(a)
            val content =
              b.split(" bags?[,.]")
                .to(List)
                .map(_.trim)
                .filter(_.size>0)
                .map(makeBagCount)
                .toMap
            Rule(bag, content)
        }
      }.toList
    }

    def containingShinyBagCount(rules:List[Rule]):Int = {
      val shiny = Bag("shiny", "gold")
      val allBags = (rules.map(_.lhs) ++ rules.flatMap(_.rhs.keys)).toSet
      def containsShiny(fromBag:Bag):Boolean = {
        if (fromBag==shiny) true
        else {
          rules.filter(_.lhs == fromBag).exists { rule =>
            rule.rhs.keys.exists(bag => containsShiny(bag))
          }
        }
      }
      (allBags - shiny).count(containsShiny)
    }

    def solve(input: String): Int = {
      val rules = parseRules(input)
      containingShinyBagCount(rules)
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {
    import Part1._

    def topoCount(rules:List[Rule]):Int = {
      val shiny = Bag("shiny", "gold")
      def worker(fromBag:Bag):Int = {
        rules.filter(_.lhs == fromBag).map{rule =>
          rule.rhs.map{case (bag,count) => count + count*worker(bag)}.sum
        }.sum
      }
      worker(shiny)
    }


    def solve2(input: String): Int = {
      val rules = parseRules(input)
      topoCount(rules)
    }
  }

}

// =====================================================================================

class PuzzleDay7Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "rules RE" should "parse entries" in {
    import PuzzleDay7.Part1._
    val content = resourceContent("day7/input-example-1.txt")
    parseRules(content) should not be empty
  }

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay7.Part1._
    solve(resourceContent("day7/input-example-1.txt")) shouldBe 4
  }
  it should "give the right result on the input file" in {
    import PuzzleDay7.Part1._
    solve(resourceContent("day7/input-given-1.txt")) should not be 287
    solve(resourceContent("day7/input-given-1.txt")) should not be 20
    solve(resourceContent("day7/input-given-1.txt")) shouldBe 211
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay7.Part2._
    solve2(resourceContent("day7/input-example-1.txt")) shouldBe 32
    solve2(resourceContent("day7/input-example-2.txt")) shouldBe 126
  }
  it should "give the right result on the input file" in {
    import PuzzleDay7.Part2._
    solve2(resourceContent("day7/input-given-1.txt")) shouldBe 12414
  }

}

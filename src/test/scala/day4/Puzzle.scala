package day4

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

object PuzzleDay4 {

  object Part1 {

    val requiredKeys: Iterable[String] = Map(
      "byr" -> "Birth Year",
      "iyr" -> "Issue Year",
      "eyr" -> "Expiration Year",
      "hgt" -> "Height",
      "hcl" -> "Hair Color",
      "ecl" -> "Eye Color",
      "pid" -> "Passport ID",
      //"cid"->"Country ID"
    ).keys

    def criteria(passport: List[(String, String)]): Boolean = {
      val keys = passport.map { case (k, _) => k }
      keys.distinct.size == keys.size && requiredKeys.forall(k => keys.contains(k))
    }

    def solve(input: String): Int = {
      val passportEntries =
        input
          .split("""\n\s*\n""")
          .to(List)

      val passportPairs =
        passportEntries
          .map(_.replaceAll("\n", " ").replaceAll("\\s+", " "))
          .map(_.split(" ").to(List).map {
            case s"$k:$v" => k -> v
          })

      passportPairs.count(criteria)
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {

    private val NumRE = """(\d+)""".r
    def between(str: String, min: Int, max: Int): Boolean = {
      str match {
        case NumRE(v) => v.toInt >= min && v.toInt <= max
        case _ => false
      }
    }

    private val HeightRE="""(\d+)((?:cm)|(?:in))""".r
    def heightControl(str: String):Boolean = {
      str match {
        case HeightRE(h,u) if u == "cm" => h.toInt >= 150 && h.toInt <=193
        case HeightRE(h,u) if u == "in" => h.toInt >= 59 && h.toInt <=76
        case _ => false
      }
    }

    val requirements:Map[String, String=>Boolean] = Map(
      "byr" -> (in => between(in, 1920,2002)), //"Birth Year",
      "iyr" -> (in => between(in, 2010, 2020)), //"Issue Year",
      "eyr" -> (in => between(in, 2020, 2030)), //"Expiration Year",
      "hgt" -> (in => heightControl(in)), //"Height",
      "hcl" -> (in => in.matches("#[0-9a-f]{6}")), //"Hair Color",
      "ecl" -> (in => List("amb","blu","brn","gry","grn","hzl","oth").contains(in)), //"Eye Color",
      "pid" -> (in => in.matches("[0-9]{9}")), //"Passport ID",
      "cid" -> (_ => true)// "Country ID"
    )

    def criteria(passport: List[(String, String)]): Boolean = {
      val keys = passport.map { case (k, _) => k }
      val mandatoryKeys = requirements.keys.filterNot(_ == "cid")

      keys.distinct.size == keys.size &&
        mandatoryKeys.forall(k => keys.contains(k)) &&
        passport.forall{ case (k, v) => requirements(k)(v)}
    }


    def solve(input: String): Int = {
      val passportEntries =
        input
          .split("""\n\s*\n""")
          .to(List)

      val passportPairs =
        passportEntries
          .map(_.replaceAll("\n", " ").replaceAll("\\s+", " "))
          .map(_.split(" ").to(List).map {
            case s"$k:$v" => k -> v
          })

      passportPairs.count(criteria)
    }
  }
}

// =====================================================================================

class PuzzleDay4Test extends AnyFlatSpec with should.Matchers with Helpers {

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay4.Part1._
    solve(resourceContent("day4/input-example-1.txt")) shouldBe 2
  }

  it should "give the right answer on the given file" in {
    import PuzzleDay4.Part1._
    solve(resourceContent("day4/input-given-1.txt")) shouldBe 202
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay4.Part2._
    solve(resourceContent("day4/input-example-1.txt")) shouldBe 2
  }

  it should "give the right answer on the given file" in {
    import PuzzleDay4.Part2._
    requirements("byr")("2000") shouldBe true
    requirements("byr")("1900") shouldBe false
    requirements("iyr")("2010") shouldBe true
    requirements("iyr")("2009") shouldBe false
    requirements("eyr")("2030") shouldBe true
    requirements("eyr")("2031") shouldBe false
    requirements("hgt")("150cm") shouldBe true
    requirements("hgt")("149cm") shouldBe false
    requirements("hgt")("76in") shouldBe true
    requirements("hgt")("77in") shouldBe false
    requirements("hcl")("#000000") shouldBe true
    requirements("hcl")("#00000z") shouldBe false
    requirements("hcl")("#00000") shouldBe false
    requirements("ecl")("amb") shouldBe true
    requirements("ecl")("zzz") shouldBe false
    requirements("pid")("123456789") shouldBe true
    requirements("pid")("12345678") shouldBe false
    requirements("pid")("1234567890") shouldBe false
    solve(resourceContent("day4/input-given-1.txt")) should not be 101
    solve(resourceContent("day4/input-given-1.txt")) should not be 188
    solve(resourceContent("day4/input-given-1.txt")) shouldBe 137
  }

}

package day21

// solved in XXmn

import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.math._

import scala.annotation.tailrec

object PuzzleDay21 {
  type Ingredient=String
  type Allergen=String
  case class Recipe(ingredients: Set[Ingredient], allergens: Set[Allergen])

  val inputRE = """([ a-z]+) [(]contains ([ a-z,]+)[)]""".r

  def parse(input: String): Array[Recipe] = {
    input
      .trim
      .split("\n")
      .map { case inputRE(rawIngredients, rawAllergens) => Recipe(rawIngredients.split(" ").toSet, rawAllergens.split(", ").toSet) }
  }

  def computeIngredientWithAllergens(recipes:Array[Recipe], remainingAllergens:Set[Allergen], candidates:Map[Allergen,Ingredient]):Map[Allergen,Ingredient] = {
    if (remainingAllergens.isEmpty) candidates else {
      val solved = candidates.values.toSet
      val reducingCandidates =
        remainingAllergens.map{allergen =>
          allergen -> recipes.filter(_.allergens.contains(allergen)).map(_.ingredients.filterNot(solved.contains)).reduce(_ intersect _)
        }
      val newCandidates = reducingCandidates.collect { case (allergen, ingredients) if ingredients.size == 1 => allergen -> ingredients.head }.toMap
      computeIngredientWithAllergens(recipes, remainingAllergens -- candidates.keys,candidates ++ newCandidates)
    }
  }

  object Part1 {

    def solve(input: String): Long = {
      val recipes = parse(input)
      val allergens = recipes.flatMap(_.allergens).toSet

      val ingredientByAllergens = computeIngredientWithAllergens(recipes, allergens, Map.empty)
      println("ingredientByAllergens : "+ingredientByAllergens)
      val ingredientsWithAllergens = ingredientByAllergens.values.toSet
      val ingredients = recipes.flatMap(_.ingredients).toSet
      val ingredientsWithoutAllergens =
        ingredients--ingredientsWithAllergens
      println("ingredientsWithoutAllergens : "+ingredientsWithoutAllergens)

      recipes.flatMap(_.ingredients).count(ingredientsWithoutAllergens)
    }
  }

  // -------------------------------------------------------------------------

  object Part2 {

    def solve(input: String): String = {
      val recipes = parse(input)
      val allergens = recipes.flatMap(_.allergens).toSet
      val ingredientByAllergens = computeIngredientWithAllergens(recipes, allergens, Map.empty)
      println("ingredientByAllergens : "+ingredientByAllergens)
      ingredientByAllergens.toList.sortBy{case (a,i) => a}.map{case (a,i) =>i}.mkString(",")
    }

  }

}

// =====================================================================================

class PuzzleDay21Test extends AnyFlatSpec with should.Matchers with Helpers {

  import PuzzleDay21._

  "parsing" should "give recipes" in {
    val parsed =
      parse(
        """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
          |trh fvjkl sbzzf mxmxvkd (contains dairy)
          |""".stripMargin
      )
    parsed.size shouldBe 2
    parsed.head.ingredients should contain allOf("mxmxvkd","kfcds","sqjhc","nhms")
    parsed.head.allergens should contain allOf("dairy","fish")
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#1 example" should "give the right result on the example" in {
    import PuzzleDay21.Part1._
    solve(resourceContent("day21/input-example-1.txt")) shouldBe 5
  }
  it should "give the right result on the input file" in {
    import PuzzleDay21.Part1._
    solve(resourceContent("day21/input-given-1.txt")) shouldBe 2734
  }

  // ------------------------------------------------------------------------------------

  "puzzle star#2 example" should "give the right result on the example" in {
    import PuzzleDay21.Part2._
    solve(resourceContent("day21/input-example-1.txt")) shouldBe "mxmxvkd,sqjhc,fvjkl"
  }
  it should "give the right result on the input file" in {
    import PuzzleDay21.Part2._
    solve(resourceContent("day21/input-given-1.txt")) shouldBe "kbmlt,mrccxm,lpzgzmk,ppj,stj,jvgnc,gxnr,plrlg"
  }
}

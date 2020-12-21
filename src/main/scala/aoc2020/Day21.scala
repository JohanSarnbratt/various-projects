package aoc2020

object Day21 {
  def run(): Unit = {
    part1("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data21test")
    part1("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data21")
  }

  def part1(fileName: String) = {
    val data = AocHelpers.readLines(fileName)
    val parsedData = data.map {
      line =>
        val parts = line
          .replace("(","")
          .replace(")","")
          .split(" contains ")
        (parts.head.split(" ").toSeq,
          parts.last.split(", ").toSeq)
    }
    println(parsedData)
    val ingredients = parsedData.flatMap(_._1).toSet
    val allergens = parsedData.flatMap(_._2).toSet
    println(ingredients)
    println(allergens)
    val x = allergens.map {
      allergen =>
        parsedData.filter(_._2.contains(allergen)).map(_._1).reduce(_.intersect(_))
    }
    println(x)
    println(ingredients -- x.flatten)
    val ingredientsWithoutAllergensGuaranteed = ingredients -- x.flatten
    println(s"part 1: ${parsedData.map(_._1.count(ingredientsWithoutAllergensGuaranteed.contains)).sum}")
    println(x.toSeq.sortBy(_.length).mkString("\n"))

    println(x.flatten.toSeq.sorted.mkString(","))
    val y = allergens.toSeq.map {
      allergen =>
        (allergen, parsedData.filter(_._2.contains(allergen)).map(_._1).reduce(_.intersect(_)))
    }.sortBy(_._2.length).foldLeft(Seq.empty[(String, Set[String])]) { //Do this step twice because we may not get the list in correct order to deduce everything the first time
      case (allergenIngredients, (allergen, ingredients)) =>
        val thisIngredient = (ingredients.toSet -- allergenIngredients.flatMap(s => if(s._2.size > 1) Set() else s._2))
        allergenIngredients.appended((allergen, thisIngredient))
    }.sortBy(_._2.size).foldLeft(Seq.empty[(String, String)]) {
      case (allergenIngredients, (allergen, ingredients)) =>
        val thisIngredient = (ingredients -- allergenIngredients.map(_._2)).head
        allergenIngredients.appended((allergen, thisIngredient))
    }
    println(y)
    println(y.sortBy(_._1).map(_._2).mkString(","))

  }
  def part2(fileName: String, startPieceName: Int): Unit = {
  }

}

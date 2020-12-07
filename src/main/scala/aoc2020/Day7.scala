package aoc2020

import scala.collection.mutable

object Day7 {

  def run() = { //part 2
    val data = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data7")
    val l = data.length
    println(l)
    val bagMap: mutable.Map[String, Seq[(Int, String)]] = mutable.Map(data.flatMap(parseLine): _*)
    var specialBags: mutable.Seq[String] = mutable.Seq("shiny gold")
    var ind = 0
    while (ind < specialBags.length) {
      val specialBag = specialBags(ind)
      bagMap.foreach {
        case (outerBag, innerBags) =>
          if (innerBags.exists(b => b._2 == specialBag) && !specialBags.contains(outerBag))
            specialBags = specialBags.appended(outerBag)
      }
      ind = ind + 1
    }
    println(specialBags.length - 1)
    println(countBags("shiny gold", bagMap))

  }

  def countBags(bag: String, bagMap: mutable.Map[String, Seq[(Int, String)]]): Int = {
    bagMap(bag).foldLeft(0)((b: Int, intstring: (Int, String)) => b + intstring._1 + intstring._1 * countBags(intstring._2, bagMap))
  }

  def parseLine(line: String): mutable.Map[String, Seq[(Int, String)]] = {
    // striped lavender bags contain 4 faded plum bags, 2 dark tomato bags, 2 plaid teal bags.
    val bags = line.split("bags contain")
    if (bags.length == 1) {
      mutable.Map(bags.head.trim -> Seq())
    } else if (bags(1).contains("no other bags")) {
      mutable.Map(bags.head.trim -> Seq())
    }
    else {
      val innerBags = bags(1).split(",")
      //println(innerBags.toSeq)
      val formInners = innerBags.map {
        innerBag =>
          val b: Array[String] = innerBag.split(" ").filterNot(_.isBlank)
          //println(b.toSeq)
          (Integer.parseInt(b.head), s"${b(1)} ${b(2)}")
      }
      mutable.Map(bags.head.trim -> formInners)
    }
  }



}

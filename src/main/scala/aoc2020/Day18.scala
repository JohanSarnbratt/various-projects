package aoc2020

object Day18 {

  def run() = { //part 2
    val data = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data18")
    println(evalPars("1 + (2 * 3) + (4 * (5 + 6))") == 51)
    println(evalPars("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") == 13632)
    println(data.map(evalPars(_)).sum)
    println(evalPars("1 + (2 * 3) + (4 * (5 + 6))", part2 = true) == 51)
    println(evalPars("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", part2 = true) == 23340)
    println(evalPars("2 * 1 + 3", part2 = true))
    print(data.map(evalPars(_, true)).sum)
  }
  def evalPars(string: String, part2: Boolean = false): Long = {
    val evaluater: List[String] => Long = if (part2) eval2 else eval
    //println(s"evaling: $string")
    if (string.contains("(")) {
      val i1 = string.zipWithIndex.findLast(_._1 == '(').get._2
      val i2 = string.drop(i1 + 1).zipWithIndex.find(_._1 == ')').get._2
      val res = evaluater(string.slice(i1 + 1, i1 + 1 + i2).split(" ").toList)
      evalPars(string.take(i1) + res + string.drop(i1 + 1).drop(i2 + 1 ), part2)
    } else {
      evaluater(string.split(" ").toList)
    }
  }
  def eval(exp: List[String]): Long = {
    exp match {
      case num1 :: "+" :: num2 :: rest => eval((java.lang.Long.parseLong(num1)+java.lang.Long.parseLong(num2)).toString :: rest)
      case num1 :: "*" :: num2 :: rest => eval((java.lang.Long.parseLong(num1)*java.lang.Long.parseLong(num2)).toString :: rest)
      case num1 :: Nil =>  java.lang.Long.parseLong(num1)
    }
  }
  def eval2(exp: List[String]): Long = {
    exp match {
      case num1 :: "+" :: num2 :: rest => eval2((java.lang.Long.parseLong(num1)+java.lang.Long.parseLong(num2)).toString :: rest)
      case num1 :: "*" :: num2 :: rest => java.lang.Long.parseLong(num1)*eval2(num2:: rest)
      case num1 :: Nil =>  java.lang.Long.parseLong(num1)
    }
  }
}

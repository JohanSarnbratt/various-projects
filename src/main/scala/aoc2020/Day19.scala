package aoc2020

object Day19 {

  def run() = { //part 2
    val testData: Seq[Seq[String]] = AocHelpers.readDataGroupsSeparatedByBlankLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data19")
    val (rules, messages) = parseData(testData)
    println(rules)
    println(messages)
    println(messages.length)
    val matchedMessages = messages.map {
      message =>
        val res = matchRule(message, Seq(rules.head), rules)
        //println(res)
        res
    }
    println(matchedMessages.map(_.count(_ == "")).count(_ > 0))
    val rules2 = rules
      .updated(8, RecursivRule(Seq(Seq(42),Seq(42,8))))
      .updated(11, RecursivRule(Seq(Seq(42, 31),Seq(42,11,31))))
    val matchedMessages2 = messages.map {
      message =>
        val res = matchRule(message, Seq(rules2.head), rules2)
        //println(res)
        res
    }
    println(matchedMessages2.map(_.count(_ == "")).count(_ > 0))
  }
  sealed trait Rule
  case class RecursivRule(rules: Seq[Seq[Int]]) extends Rule
  case class CharacterRule(char: Char) extends Rule
  def parseData(data: Seq[Seq[String]]) = {
    val rules = data.head.map { rulesString =>
      val s1 = rulesString.split(": ")
      val ruleName: Int = Integer.parseInt(s1.head)
      val rule: Rule = if (s1(1).contains("\""))
        CharacterRule(s1(1)(1))
      else {
        //println(s1(1))
        val s2 = s1(1).replace('|', 'x').split(""" x """).toSeq
        //println(s"s2: $s2")
        RecursivRule(s2.map(_.split(" ").toSeq.map(Integer.parseInt)))
      }
      (ruleName, rule)
    }.sortBy(_._1).map(_._2)

    (rules, data(1))
  }
  def matchRule(message: String, currentRules: Seq[Rule], rules: Seq[Rule]): Seq[String] = {
    val x = currentRules.toList match {
      case Nil => Seq(message)
      case CharacterRule(c) :: nextRules =>
        if (message.length > 0 && message.head == c)
          matchRule(message.tail, nextRules, rules)
        else
          Seq.empty
      case RecursivRule(subRules) :: nextRules =>
        val possibleNextMessages = subRules.flatMap(subRule => matchRule(message, subRule.map(rules(_)), rules))
        possibleNextMessages.flatMap {
          nextMessage => matchRule(nextMessage, nextRules, rules)
        }
    }
    x.distinct
  }
}

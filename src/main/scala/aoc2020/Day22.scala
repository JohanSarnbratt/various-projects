package aoc2020

import scala.collection.mutable

object Day22 {
  def run(): Unit = {
    println("Test: ")
    part1("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data22test")
    println("Part 1: ")
    part1("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data22")
    println("Test 2: ")
    part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data22test")
    println("Test 2b: ")
    part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data22test2")
    println("part 2: ")
    part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data22")
  }

  def part1(fileName: String) = {
    val data = AocHelpers.readDataGroupsSeparatedByBlankLines(fileName)
    var p1 = data.head.tail.map(Integer.parseInt)
    var p2 = data.last.tail.map(Integer.parseInt)
    println(p1)
    println(p2)
    var round = 0
    while(p1.nonEmpty && p2.nonEmpty) {
      val (r1, r2) = step(p1, p2)
      p1 = r1
      p2 = r2
      round = round+1
      if (round % 10000 == 0)
        println(s"Round: $round ${p1.length} ${p2.length}")
    }
    println(p1)
    println(s"score1: ${score(p1)}")
    println(p2)
    println(s"score2: ${score(p2)}")
    println(round)
  }
  def part2(fileName: String) = {
    val data = AocHelpers.readDataGroupsSeparatedByBlankLines(fileName)
    var p1 = data.head.tail.map(Integer.parseInt)
    var p2 = data.last.tail.map(Integer.parseInt)
    println(p1)
    println(p2)
    println(recursiveCombat(p1, p2, 0))
  }
  def recursiveCombat(ibp1: Seq[Int], ibp2: Seq[Int], depth: Int): (Boolean, Int) = {
    println(s"STarting game at depth $depth")
    var p1 = ibp1
    var p2 = ibp2
    var round = 0
    val previousStates = mutable.Map.empty[(Seq[Int], Seq[Int]), Boolean].withDefaultValue(false)
    while(p1.nonEmpty && p2.nonEmpty && !previousStates((p1, p2))) {
      previousStates.put((p1, p2), true)
      if(p1.head <= p1.tail.length && p2.head <= p2.tail.length) {
        val (p1won, _) = recursiveCombat(p1.tail.take(p1.head), p2.tail.take(p2.head), depth+1)
        if (p1won) {
          p1 = p1.tail.appended(p1.head).appended(p2.head)
          p2 = p2.tail
        } else {
          p2 = p2.tail.appended(p2.head).appended(p1.head)
          p1 = p1.tail
        }
      } else if (p1.head > p2.head) {
        p1 = p1.tail.appended(p1.head).appended(p2.head)
        p2 = p2.tail
      } else {
        p2 = p2.tail.appended(p2.head).appended(p1.head)
        p1 = p1.tail
      }
      round = round+1
      if (round % 10000 == 0)
        println(s"Round: $round ${p1.length} ${p2.length}")
    }
    if (p1.isEmpty) {
      (false, score(p2))
    } else {
      (true, score(p1))
    }
  }
  def step(p1: Seq[Int], p2: Seq[Int]) = {
    if (p1.head > p2.head)
      (p1.tail.appended(p1.head).appended(p2.head),
      p2.tail)
    else
      (p1.tail,
        p2.tail.appended(p2.head).appended(p1.head))

  }
  def score(list: Seq[Int]): Int = list match {
    case Nil => 0
    case f :: tail => f*list.length + score(tail)
  }

}

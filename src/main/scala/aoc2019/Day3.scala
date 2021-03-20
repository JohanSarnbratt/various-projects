package aoc2019

import aoc2020.AocHelpers

import scala.collection.immutable

object Day3 {
  def run(): Unit = {
    val testData = AocHelpers.readDataGroupsSeparatedByBlankLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2019/data3test")

    println("Test 1: ")
    testData.foreach(part1)
    println("Part 1: ")
    val data = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2019/data3")
    part1(data)
    println("Test 2: ")
    testData.foreach(part2)
    println("Part 2: ")
    part2(data)
  }

  def part1(data: Seq[String]) = {
    val (lines1,lines2) = parseLines(data)

    val crossings = lines1.flatMap(line1 => lines2.flatMap(line2 => crossing(line1, line2))).filterNot(_ == (0,0))
    println(crossings.map{case (x,y) => Math.abs(x)+Math.abs(y)}.min)
  }

  private def parseLines(data: Seq[String]): (Seq[((Int, Int), (Int, Int))], Seq[((Int, Int), (Int, Int))]) = {
    val res = data.map {
      str =>
        str.split(",").foldLeft(List((0, 0))) {
          case ((x, y) :: tail, str) =>
            val value = Integer.parseInt(str.tail)
            str.head match {
              case 'R' => (x + value, y) :: (x, y) :: tail
              case 'L' => (x - value, y) :: (x, y) :: tail
              case 'U' => (x, y + value) :: (x, y) :: tail
              case 'D' => (x, y - value) :: (x, y) :: tail
            }
        }
    }
    val lines1: Seq[((Int, Int), (Int, Int))] = res.head.sliding(2).map(l => (l(1), l.head)).toSeq.reverse
    val lines2: Seq[((Int, Int), (Int, Int))] = res.last.sliding(2).map(l => (l(1), l.head)).toSeq.reverse
    (lines1,lines2)
  }

  def crossing(line1: ((Int, Int), (Int, Int)), line2: ((Int, Int), (Int, Int))): Option[(Int, Int)] = {
    //line1 vertical line2 horizontal
    val (x, y) = if (line1._1._1 == line1._2._1 && line2._1._2 == line2._2._2) {
      (line1._1._1, line2._1._2)
    } else if
    //line1 horizontal line2 vertical
    (line1._1._2 == line1._2._2 && line2._1._1 == line2._2._1) {
      (line2._1._1, line1._1._2)
    } else (0,0)
    if (
      x <= Math.max(line1._1._1, line1._2._1) &&
        x <= Math.max(line2._1._1, line2._2._1) &&
        x >= Math.min(line1._1._1, line1._2._1) &&
        x >= Math.min(line2._1._1, line2._2._1) &&
        y <= Math.max(line1._1._2, line1._2._2) &&
        y <= Math.max(line2._1._2, line2._2._2) &&
        y >= Math.min(line1._1._2, line1._2._2) &&
        y >= Math.min(line2._1._2, line2._2._2)
    ) {
      Some((x, y))
    }
    else
      None
  }
  def part2(data: Seq[String]) = {
    val (lines1,lines2) = parseLines(data)
    println(lines1)
    println(lines2)
    var len1 = 0
    val crossings1: Seq[((Int, Int), Int)] = lines1.flatMap(line1 => {
      val crossings = lines2.flatMap(line2 => crossing2(line1, line2, len1))
      len1 = len1 + lengthOfLine(line1)
      crossings
    }).filterNot(_._1 == (0,0))
    var len2 = 0
    val crossings2: Seq[((Int, Int), Int)] = lines2.flatMap(line2 => {
      val crossings = lines1.flatMap(line1 => crossing2(line2, line1, len2))
      len2 = len2 + lengthOfLine(line2)
      crossings
    }).filterNot(_._1 == (0,0))
    println(crossings1)
    println(crossings2)
    val lengthToCrossings: Map[(Int, Int), Int] = (crossings1++crossings2).groupBy(_._1).map{
      case (coords, Seq((_, l1), (_,l2))) => (coords, l1+l2)
      case x => throw new RuntimeException(s"Did not match expected pattern: $x")
    }
    println(s"Result: ${lengthToCrossings.minBy(_._2)}")
  }
  def lengthOfLine(line: ((Int, Int), (Int, Int))) = Math.abs(line._1._1-line._2._1)+Math.abs(line._1._2-line._2._2)

  def crossing2(line1: ((Int, Int), (Int, Int)), line2: ((Int, Int), (Int, Int)), dist: Int): Option[((Int, Int), Int)] = {
    //line1 vertical line2 horizontal
    val (x, y) = if (line1._1._1 == line1._2._1 && line2._1._2 == line2._2._2) {
      (line1._1._1, line2._1._2)
    } else if
    //line1 horizontal line2 vertical
    (line1._1._2 == line1._2._2 && line2._1._1 == line2._2._1) {
      (line2._1._1, line1._1._2)
    } else (0,0)
    if (
      x <= Math.max(line1._1._1, line1._2._1) &&
        x <= Math.max(line2._1._1, line2._2._1) &&
        x >= Math.min(line1._1._1, line1._2._1) &&
        x >= Math.min(line2._1._1, line2._2._1) &&
        y <= Math.max(line1._1._2, line1._2._2) &&
        y <= Math.max(line2._1._2, line2._2._2) &&
        y >= Math.min(line1._1._2, line1._2._2) &&
        y >= Math.min(line2._1._2, line2._2._2)
    ) {
      Some(((x, y), dist+Math.abs(line1._1._1-x)+Math.abs(line1._1._2-y)))
    }
    else
      None
  }

}
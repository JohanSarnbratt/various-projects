package aoc2019

import aoc2020.AocHelpers

object Day3 {
  def run(): Unit = {
    val testData = AocHelpers.readDataGroupsSeparatedByBlankLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2019/data3test")

    println("Test 1: ")
    testData.foreach(part1)
    println("Part 1: ")
    val data = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2019/data3")
    part1(data)
    println("Part 2: ")
  }

  def part1(data: Seq[String]) = {
    val res = data.map {
      str => str.split(",").foldLeft(List((0,0))) {
        case ((x,y) :: tail , str) =>
          val value = Integer.parseInt(str.tail)
          str.head match {
            case 'R' => (x+value,y) :: (x,y) :: tail
            case 'L' => (x-value,y) :: (x,y) :: tail
            case 'U' => (x,y+value) :: (x,y) :: tail
            case 'D' => (x,y-value) :: (x,y) :: tail
          }
      }
    }
    val lines1: Seq[((Int, Int), (Int, Int))] = res.head.sliding(2).map(l => (l.head, l(1))).toSeq
    val lines2 = res.last.sliding(2).map(l => (l.head, l(1))).toSeq

    val crossings = lines1.flatMap(line1 => lines2.flatMap(line2 => crossing(line1, line2))).filterNot(_ == (0,0))
    println(crossings.map{case (x,y) => Math.abs(x)+Math.abs(y)}.min)
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
  def part2(fileName: String) = {
    val data = AocHelpers.readLines(fileName)
  }

}
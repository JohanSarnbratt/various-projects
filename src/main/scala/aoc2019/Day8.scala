package aoc2019

import aoc2020.AocHelpers

object Day8 {
  def run() = {
    val data = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2019/data8")
    //println("Test 1: ")
    //part1(testData)
    println("Part 1: ")
    part1(data)
    println("Part 2: ")
    part2(data)
  }
  def part1(data: Seq[String]) = {
    val img = data.head
    val layers = img.grouped(25*6).toList
    val scores: Seq[Int] = layers.map(layer => Seq('0','1','2').map(c => layer.count(_ == c))).minBy(_.head)
    //println(scores)
    println(scores(1)*scores(2))
  }
  def part2(data: Seq[String]) = {
    val img = data.head
    val layers = img.grouped(25*6).toSeq
    val finalLayer: Seq[Char] = (0 until 25*6)
      .map(index => layers.find(layer => layer(index) != '2').get(index)).mkString
    finalLayer.map{
      case '0' => ' '
      case '1' => '#'
    }.mkString.grouped(25).toSeq.map(println)
  }

}

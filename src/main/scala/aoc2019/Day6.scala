package aoc2019

import aoc2020.AocHelpers

import scala._

object Day6 {
  def run(): Unit = {
    val testData = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2019/data6test")
    val testData2 = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2019/data6test2")
    val data = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2019/data6")
    println("Test 1: ")
    part1(testData)
    println("Part 1: ")
    part1(data)
    println("Test 2: ")
    part2(testData2)
    println("Part 2: ")
    part2(data)
  }
  def parseData(data: Seq[String]) = {
    data.map {
      line =>
        val planets = line.split(')')
        planets.head -> planets.last
    }
  }

  def part1(data: Seq[String]): Unit = {
    val orbs = parseData(data)
    def recursiveCount(planet: String): (Int, Int) = {
      val (r,o) = orbs.filter {
        case (orbitee, _) if planet == orbitee => true
        case _ => false
      }.map {
        case (_, orbiter) =>
          //println(s"$p $orbiter")
          recursiveCount(orbiter)
      }.fold((0,0)) {
        case ((planets1, orbits1), (planets2, orbits2)) => (planets1+planets2, orbits1+orbits2)
      }
      (r+1,r+o)
    }
    //println(orbs)
    println(recursiveCount("COM"))
    //println(recursiveCount("F"))
    //orbs.foreach((planet) => println(s"${planet._1}: ${recursiveCount(planet._1)}"))
  }
  def part2(data: Seq[String]): Unit = {
    val orbs = parseData(data)
    def parents(planet: String): Seq[String] = {
      orbs.flatMap {
        case (dom, sub) if sub == planet => parents(dom).appended(planet)
        case _ => Seq.empty
      }
    }

    val youParents = parents("YOU")
    val sanParents = parents("SAN")
    //println(youParents)
    //println(sanParents)
    val r = (youParents++sanParents).distinct.size*2-youParents.size-sanParents.size-2
    println(r)
  }
}
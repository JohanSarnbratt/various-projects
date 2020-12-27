package aoc2019

import aoc2020.AocHelpers

object Day1 {
  def run(): Unit = {
    println("Test: ")
    //println(part1("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data1test"))
    val weight = part1("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2019/data1")
    println(weight)
    println(fuelForFuel(weight))
    println(fuelForFuel(weight))
    println(part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2019/data1"))
  }

  def part1(fileName: String) = {
    val data = AocHelpers.readLines(fileName).map(Integer.parseInt).map(_/3-2).sum
    data
  }
  def part2(fileName: String) = {
    val data = AocHelpers.readLines(fileName).map(Integer.parseInt).map(_/3-2).map(x => x + fuelForFuel(x)).sum
    data
  }
  def fuelForFuel(weight: Int): Int = {
    val fuel = Math.max(weight/3-2,0)
    if (fuel == 0)
      fuel
    else
      fuel+fuelForFuel(fuel)
  }

}

package aoc2019

import aoc2020.AocHelpers

object Day4 {
  def run(): Unit = {
    //checkNumWithExactDouble(277789)
    part1(273025, 767253)
    part2(273025, 767253)
    //part2(data)
  }

  def part1(low: Int, high: Int): Int = {
    val res = Seq.range(low, high+1).map(checkNum(_)).count(x => x)
    println(res)
    res
  }
  def checkNum(num: Int, lastDig: Int = 10, hasDouble: Boolean = false): Boolean = {
    val currentDig = num%10
    if (num == 0) {
      hasDouble
    } else if (currentDig == lastDig) {
      checkNum(num/10, currentDig, hasDouble = true)
    } else if (currentDig < lastDig) {
      checkNum(num/10, currentDig, hasDouble)
    } else {
      false
    }
  }
  def part2(low: Int, high: Int): Int = {
    val res = Seq.range(low, high+1).map {
      num => val r = checkNumWithExactDouble(num)
        if (r) println(num)
        r
    }.count(x => x)
    println(res)
    res
  }
  def checkNumWithExactDouble(num: Int, lastDig: Int = 10, streak: Int = 1, hasDouble: Boolean = false): Boolean = {
    //println(s"${num} ${lastDig} ${streak} ${hasDouble} ")
    val currentDig = num%10
    if (num == 0) {
      hasDouble || streak == 2
    } else if (currentDig == lastDig) {
      checkNumWithExactDouble(num/10, currentDig, streak+1, hasDouble)
    } else if (currentDig < lastDig && streak == 2) {
      checkNumWithExactDouble(num/10, currentDig, 1, true)
    } else if (currentDig < lastDig) {
      checkNumWithExactDouble(num/10, currentDig, 1, hasDouble)
    } else {
      false
    }
  }

}
package aoc2020

import scala.collection.mutable

object Day15 {

  def run() = { //part 1
    val dataTest1 = Seq(0,3,6)
    val data = Seq(0,1,5,10,3,12,19)
    val part1goal = 30*1000*1000
    println(game(data, 2020) == 1373)
    println(game2(data, 2020) == 1373)
    //println(AocHelpers.time(game(dataTest1,part1goal)))
    println(AocHelpers.time(game2(dataTest1, part1goal)) == 175594)
    //println(AocHelpers.time(game(data,part1goal)))
    println(AocHelpers.time(game2(data, part1goal)))
    //println(game2(dataTest1, 30000000) == 436)
    //println(game2(data, 30000000))
  }
  def game(init: Seq[Int], goalInd: Int) = {
    var s = init
    while (s.length < goalInd) {
      s.lastIndexOf(s.last, s.length-2) match {
        case -1 => s = s.appended(0)
        case ind => s = s.appended(s.length-ind-1)
      }
    }
    //println(s)
    s(goalInd-1)
  }
  def game2(init: Seq[Int], goalInd: Int) = {
    var s = init
    val distances: mutable.Map[Int, Int] = mutable.Map(init.init.zipWithIndex.map{
      case (num, i) => num -> i
    }: _*)
    var lastDig = init.last
    var steps = init.length-1
    while (steps < goalInd-1) {
      if (distances.contains(lastDig)) {
        val newDig = steps - distances(lastDig)
        distances(lastDig) = steps
        lastDig = newDig
      } else {
        distances(lastDig) = steps
        lastDig = 0
      }
      steps = steps + 1
      //s = s.appended(lastDig)
      //println(s"lastDig: $lastDig")
      //println(s"distances: $distances")
      //println(s"distances.size: ${distances.size}")
    }
    //println(s)
    lastDig
  }
}
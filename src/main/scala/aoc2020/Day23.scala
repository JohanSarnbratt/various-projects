package aoc2020

import java.util

import scala.collection.mutable

object Day23 {
  def run(): Unit = {
    println("Test: ")
    println(performStep(Seq(3, 8,  9,  1,  2,  5,  4,  6,  7)) == Seq(2, 8,  9,  1,  5,  4,  6,  7, 3))
    println(performStep(Seq(2, 8,  9,  1,  5,  4,  6,  7, 3)) == Seq(5, 4,  6,  7,  8,  9,  1, 3, 2))
    println(performStep(Seq(9, 7, 3, 8, 1, 6, 4, 5, 2)) == Seq(1, 6, 7, 3, 8, 4, 5, 2, 9))
    println(performStep(Seq(9, 7, 3, 8, 1, 6, 4, 5, 2)))
    //println(part1("389125467", 10) == "92658374")
    //println(part1("389125467", 100))
    //println(part1("389125467", 100) == "67384529")
    //println(part1("364289715", 100))
    println(part2("389125467", 10, 20))
    println(AocHelpers.time(part2("389125467", 10*1000*1000, 1000*1000)))
    println(AocHelpers.time(part2("364289715", 10*1000*1000, 1000*1000)))
  }

  def part1(cupString: String, steps: Int) = {
    var cups: Seq[Int] = cupString.map(_.toString).map(Integer.parseInt)
    println(cups)
    (0 until steps).foreach{_ =>
      cups = performStep(cups)
      println(cups)
    }
    val ind1 = cups.indexOf(1)
    cups = cups.slice(ind1+1, cups.length) ++ cups.take(ind1)
    cups.mkString("")
  }
  def part2(cupString: String, steps: Int, numberOfCups: Int) = {
    val cupSeq: Seq[Int] = cupString.map(_.toString).map(Integer.parseInt)
    //For performance reason we want to use something like a linked list
    //A Map[Int, Int] will work just like a linked list as long as were careful
    val cups: mutable.Map[Int, Int] = mutable.Map.empty[Int, Int]
    (0 until cupSeq.length-1).foreach{ind =>
      cups.put(cupSeq(ind), cupSeq(ind+1))
    }
    cups.put(cupSeq.last, cupSeq.max+1)
    (cupSeq.max+1 until numberOfCups).foreach{ cup =>
      cups.put(cup, cup+1)
    }
    cups.put(numberOfCups, cupSeq.head)

    //perform step in place
    var currentCup = cupSeq.head
    (0 until steps).foreach{_=>
      val moveBlock = getCupsFrom(cups(currentCup), 3, cups)
      val destination = getDestination(currentCup, moveBlock, numberOfCups)
      val destinationFollower = cups(destination)
      val nextCurrentCup = cups(moveBlock.last)

      /*println(s"cups: ${getCupsFrom(currentCup, 20, cups)}")
      println(s"currentCup: $currentCup destination: $destination moveBlock: $moveBlock")
      println()*/
      cups(currentCup) = nextCurrentCup
      cups(destination) = moveBlock.head
      cups(moveBlock.last) = destinationFollower

      currentCup = nextCurrentCup
    }
    (cups(1), cups(cups(1)), cups(1).toLong * cups(cups(1)).toLong)
  }
  def getCupsFrom(startCup: Int, take: Int, cups: mutable.Map[Int, Int]): List[Int] = {
    if (take <= 1)
      List(startCup)
    else
      startCup :: getCupsFrom(cups(startCup), take-1, cups)
  }
  def getDestination(currentCup: Int, omit: Seq[Int], numberOfCups: Int) = {
    var destination = currentCup - 1
    while (omit.contains(destination))
      destination = destination - 1
    if (destination > 0)
      destination
    else {
      destination = numberOfCups
      while (omit.contains(destination))
        destination = destination - 1
      destination
    }
  }
  def performStep(cups: Seq[Int]): Seq[Int] = {
    //    move
    //    this
    // h (* * *) * * d x * *
    // * * d (* * *) x * * h
    val currentCup = cups.head
    val destination = findLower(currentCup, cups.drop(4), cups.max)
    val destinationInd = cups.indexOf(destination)
    //println(s"cups: $cups")
    //println(s"currentCup: $currentCup destination: $destination destinationInd: $destinationInd")
    //println(s"res: ${cups.slice(4, destinationInd+1) ++ cups.slice(1,4) ++ cups.slice(destinationInd+1, cups.length) ++ Seq(cups.head)}")
    cups.slice(4, destinationInd+1) ++ cups.slice(1,4) ++ cups.slice(destinationInd+1, cups.length) ++ Seq(cups.head)
  }
  def findLower(cup: Int, cups: Seq[Int], max: Int): Int = {
    val maybeNext = (cup+max)%(max+1)
    if (cups.contains(maybeNext))
      maybeNext
    else
      findLower(maybeNext, cups, max)
  }
}

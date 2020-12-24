package aoc2020

import scala.collection.mutable

object Day24 {
  def run(): Unit = {
    println("Test: ")
    println(part1("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data24test") == 10)
    println(part1("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data24"))
    println("Part 2 test: ")
    println(part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data24test") == 2208)
    println("Part 2: ")
    println(part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data24"))
  }

  def part1(fileName: String) = {
    val data = AocHelpers.readLines(fileName)
    val cups: mutable.Map[(Int, Int), Int] = mutable.Map.empty[(Int, Int), Int].withDefaultValue(0)
    data.foreach{line =>
      val coords = parseLine(line)
      cups.put((coords._1, coords._2), (cups(coords._1, coords._2)+1)%2)
    }
    cups.count{
      case (_, value) => value == 1
    }

  }
  def parseLine(line: String): (Int, Int) = {
    if (line.isEmpty)
      (0,0)
    else {
      val coords = line match {
        case line if line.startsWith("ne") => (1, -1, line.drop(2))
        case line if line.startsWith("e") => (1, 0, line.drop(1))
        case line if line.startsWith("se") => (0, 1, line.drop(2))
        case line if line.startsWith("sw") => (-1, 1, line.drop(2))
        case line if line.startsWith("w") => (-1, 0, line.drop(1))
        case line if line.startsWith("nw") => (0, -1, line.drop(2))
      }
      val (x,y) = parseLine(coords._3)
      (coords._1+x, coords._2+y)
    }
  }
  def part2(fileName: String) = {
    val data = AocHelpers.readLines(fileName)
    val cups: mutable.Map[(Int, Int), Int] = mutable.Map.empty[(Int, Int), Int].withDefaultValue(0)
    data.foreach{line =>
      val coords = parseLine(line)
      cups.put((coords._1, coords._2), (cups(coords._1, coords._2)+1)%2)
    }
    println(countTiles(cups))
    (0 until 10).foreach{_ =>
      println(s"keys1: ${cups.keys.size}")
      touchNeis(cups)
      println(s"keys2: ${cups.keys.size}")
      step(cups)
      println(s"keys3: ${cups.keys.size}")
      println(countTiles(cups))
    }

    countTiles(cups)
  }
  def countTiles(cups: mutable.Map[(Int, Int), Int]) = {
    cups.count{
      case (_, value) => value == 1
    }
  }
  def step(cups: mutable.Map[(Int, Int), Int]) = {
    cups.keys.filter{
      case (x,y) =>
        val ns = getBlackNeighbours(x,y,cups)
        val flipBlack = cups((x,y)) == 1 && (ns == 0 || ns > 2)
        val flipWhite = cups((x,y)) == 0 && ns == 2
        flipBlack || flipWhite
    }.foreach{
      case (x,y) =>
        cups.put((x, y), (cups(x, y)+1)%2)
    }
  }
  def getBlackNeighbours(x: Int, y: Int, cups: mutable.Map[(Int, Int), Int]) = {
    cups((x+1,y)) +
      cups((x,y+1)) +
      cups((x-1,y+1)) +
      cups((x-1,y)) +
      cups((x,y-1)) +
      cups((x+1,y-1))
  }
  def touchNeis(cups: mutable.Map[(Int, Int), Int]) = {
    cups.keys.foreach {
      case (x,y) if cups((x,y)) == 0 =>
        if (cups((x+1,y)) == 0) cups.put((x+1, y), 0)
        if (cups((x,y+1)) == 0) cups.put((x,y+1), 0)
        if (cups((x-1,y+1)) == 0) cups.put((x-1,y+1), 0)
        if (cups((x-1,y)) == 0) cups.put((x-1,y), 0)
        if (cups((x,y-1)) == 0) cups.put((x,y-1), 0)
        if (cups((x+1,y-1)) == 0) cups.put((x+1,y-1), 0)
      case _ =>
    }
  }
}

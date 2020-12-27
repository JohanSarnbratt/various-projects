package aoc2020

import scala.collection.mutable

object Day24 {
  def run(): Unit = {
    println("Test: ")
    println(part1("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data24test") == 10)
    println(part1("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data24"))
    println("Part 2 test: ")
    //println(part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data24test") == 2208)
    println(part2b("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data24test") == 2208)
    println("Part 2: ")
    println(part2b("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data24"))
  }

  def part1(fileName: String) = {
    val cups: mutable.Map[(Int, Int), Int] = parseMap(fileName)
    countTiles(cups)


  }

  def parseMap(fileName: String) = {
    val data = AocHelpers.readLines(fileName)
    val cups: mutable.Map[(Int, Int), Int] = mutable.Map.empty[(Int, Int), Int].withDefaultValue(0)
    data.foreach { line =>
      parseLine(line) match {
        case (x, y) => cups.put((x, y), (cups(x, y) + 1) % 2)
      }
    }
    cups
  }

  def parseLine(line: String): (Int, Int) = {
    if (line.isEmpty)
      (0, 0)
    else {
      val coords = line match {
        case line if line.startsWith("ne") => (1, -1, line.drop(2))
        case line if line.startsWith("e") => (1, 0, line.drop(1))
        case line if line.startsWith("se") => (0, 1, line.drop(2))
        case line if line.startsWith("sw") => (-1, 1, line.drop(2))
        case line if line.startsWith("w") => (-1, 0, line.drop(1))
        case line if line.startsWith("nw") => (0, -1, line.drop(2))
      }
      val (x, y) = parseLine(coords._3)
      (coords._1 + x, coords._2 + y)
    }
  }

  def part2(fileName: String) = {
    val cups: mutable.Map[(Int, Int), Int] = parseMap(fileName)

    //println(cups)
    println(countTiles(cups))
    (0 until 10).foreach { _ =>
      //println(s"keys1: ${cups.keys.size}")
      touchNeis(cups)
      //println(cups)
      //println(s"keys2: ${cups.keys.size}")
      step(cups)
      //println(s"keys3: ${cups.keys.size}")
      //println(cups)
      println(countTiles(cups))
    }

    countTiles(cups)
  }

  def part2b(fileName: String) = {
    val cups: mutable.Map[(Int, Int), Int] = parseMap(fileName)
    println(countTiles(cups))
    val boardSize = 1000
    val board: mutable.Seq[mutable.Seq[Boolean]] = mutable.Seq.fill(boardSize)(mutable.Seq.fill(boardSize)(false))
    cups.foreach {
      case ((x, y), value) => board(boardSize / 2 + x)(boardSize / 2 + y) = value == 1
    }
    (1 to 100).foreach { day =>
      step(board)
      println(s"day: $day tiles: ${countTiles(board)}")
    }

    countTiles(board)
  }

  def countTiles(cups: mutable.Map[(Int, Int), Int]) = {
    cups.count {
      case (_, value) => value == 1
    }
  }

  def countTiles(board: mutable.Seq[mutable.Seq[Boolean]]): Int = {
    board.map {
      row => row.count(b => b)
    }.sum
  }

  def step(cups: mutable.Map[(Int, Int), Int]) = {
    cups.keys.filter {
      case (x, y) =>
        val ns = getBlackNeighbours(x, y, cups)
        val flipBlack = cups((x, y)) == 1 && (ns == 0 || ns > 2)
        val flipWhite = cups((x, y)) == 0 && ns == 2
        flipBlack || flipWhite
    }.foreach {
      case (x, y) =>
        cups.put((x, y), (cups(x, y) + 1) % 2)
    }
  }

  def step(board: mutable.Seq[mutable.Seq[Boolean]]) = {
    val changeKeys = board.indices.flatMap { x =>
      board(x).indices.flatMap { y =>
        val ns = getBlackNeighbours(x, y, board)
        val flipBlack = board(x)(y) && (ns == 0 || ns > 2)
        val flipWhite = !board(x)(y) && ns == 2
        if (flipBlack || flipWhite)
          Some((x, y))
        else
          None

      }
    }
    changeKeys.foreach {
      case (x, y) =>
        board(x)(y) = !board(x)(y)
    }
  }

  def getBlackNeighbours(x: Int, y: Int, cups: mutable.Map[(Int, Int), Int]) = {
    cups((x + 1, y)) +
      cups((x, y + 1)) +
      cups((x - 1, y + 1)) +
      cups((x - 1, y)) +
      cups((x, y - 1)) +
      cups((x + 1, y - 1))
  }

  def getBlackNeighbours(x: Int, y: Int, board: mutable.Seq[mutable.Seq[Boolean]]): Int = {
    val boardSize = board.length
    val neis: Seq[Boolean] =
      Seq(
        if (x + 1 < boardSize - 1) board(x + 1)(y) else false,
        if (y + 1 < boardSize - 1) board(x)(y + 1) else false,
        if (x - 1 > 0) board(x - 1)(y) else false,
        if (y - 1 > 0) board(x)(y - 1) else false,
        if (x + 1 < boardSize - 1 && y - 1 > 0) board(x + 1)(y - 1) else false,
        if (y + 1 < boardSize - 1 && x - 1 > 0) board(x - 1)(y + 1) else false
      )
    neis.count(b => b)
  }

  def touchNeis(cups: mutable.Map[(Int, Int), Int]) = {
    cups.keys.foreach {
      case (x, y) =>
        if (cups((x + 1, y)) == 0) cups.put((x + 1, y), 0)
        if (cups((x, y + 1)) == 0) cups.put((x, y + 1), 0)
        if (cups((x - 1, y + 1)) == 0) cups.put((x - 1, y + 1), 0)
        if (cups((x - 1, y)) == 0) cups.put((x - 1, y), 0)
        if (cups((x, y - 1)) == 0) cups.put((x, y - 1), 0)
        if (cups((x + 1, y - 1)) == 0) cups.put((x + 1, y - 1), 0)
    }
  }
}

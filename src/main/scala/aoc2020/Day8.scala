package aoc2020

import aoc2020.AssRunner.Halted

import scala.collection.mutable

object Day8 {

  def run() = { //part 2
    val data = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data8")
    val l = data.length
    println(l)
    val commands: Seq[(String, Int)] = data.map(parseLine)
    //part 1
    val ar = new AssRunner(commands)
    println(ar.run())

    //part 2
    println(time(bruteForce(commands)))
    val visitedPositions: mutable.Seq[Boolean] = mutable.Seq(commands: _*).map(_ => false)
    println(time(grena(commands, visitedPositions)))
  }
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)/1000000.0 + "ms")
    result
  }

  def bruteForce(commands: Seq[(String, Int)]): Int = {
    commands.indices.foreach {
      ind =>
        commands(ind) match {
          case ("jmp", num) =>
            val ar = new AssRunner(commands.updated(ind, ("nop", num)))
            val res = ar.run()
            res match {
              case Halted(acc, 0) =>
                println(ind)
                println(acc)
                return acc
              case _ =>
            }
          case ("nop", num) =>
            val ar = new AssRunner(commands.updated(ind, ("jmp", num)))
            val res = ar.run()
            res match {
              case Halted(acc, 0) =>
                println(ind)
                println(acc)
                return acc
              case _ =>
            }
          case _ =>
        }
    }
    -1
  }

  def grena(commands: Seq[(String, Int)], visitedPositions: mutable.Seq[Boolean], currentPosition: Int = 0, accumulator: Int = 0, redanGrenat: Boolean = false): Int = {
    if (currentPosition == commands.length) {
      accumulator
    } else if (visitedPositions(currentPosition)) {
      -1
    } else {
      visitedPositions(currentPosition) = true
      commands(currentPosition) match {
        case ("acc", num) =>
          grena(
            commands,
            visitedPositions,
            currentPosition + 1,
            accumulator + num,
            redanGrenat
          )
        case ("jmp", num) =>
          val r1 =
            grena(
              commands,
              visitedPositions,
              currentPosition + num,
              accumulator,
              redanGrenat
            )
          val r2: Int = if (!redanGrenat) {
            grena(
              commands.updated(currentPosition, ("nop", num)),
              visitedPositions,
              currentPosition + 1,
              accumulator,
              redanGrenat = true
            )
          } else {
            -1
          }
          Math.max(r1, r2)
        case ("nop", num) =>
          val r1 =
            grena(
              commands,
              visitedPositions,
              currentPosition + 1,
              accumulator,
              redanGrenat
            )
          val r2: Int = if (!redanGrenat) {
            grena(
              commands.updated(currentPosition, ("nop", num)),
              visitedPositions,
              currentPosition + 1,
              accumulator,
              redanGrenat = true
            )
          } else {
            -1
          }
          Math.max(r1, r2)
        case cmd => throw AssRunner.AssException(s"Unknown command: $cmd")
      }
    }
  }

  def parseLine(line: String): (String, Int) = {
    val bags = line.split(" ")
    (bags.head, Integer.parseInt(bags.last))
  }



}

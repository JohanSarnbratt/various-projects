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
    println(time(backwards(commands)))
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000.0 + "ms")
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

  def backwards(commands: Seq[(String, Int)]) = {
    val ar = new AssRunner(commands)
    ar.run()
    val infiniteLoop = ar.getVisitedPositions
    val visitedPositions: mutable.Seq[Boolean] = mutable.Seq(commands: _*).map(_ => false)
    val flipInd = grenaBak(commands, infiniteLoop, visitedPositions, commands.length)
    val newCommands = commands.updated(flipInd, flipCommand(commands(flipInd)))
    println(new AssRunner(newCommands).run())

  }

  def flipCommand(command: (String, Int)) =
    command match {
      case ("jmp", num) => ("nop", num)
      case ("nop", num) => ("jmp", num)
      case x => x
    }

  def grenaBak(commands: Seq[(String, Int)], infiniteLoop: mutable.Seq[Boolean], visitedPositions: mutable.Seq[Boolean], target: Int, redanGrenat: Boolean = false, changeIndex: Option[Int] = None): Int = {
    if (target < infiniteLoop.length && infiniteLoop(target)) {
      target
    } else if (target < infiniteLoop.length && visitedPositions(target)) {
      -1
    } else {
      val newVisitedPositions = visitedPositions
      if (target < infiniteLoop.length) newVisitedPositions(target) = true

      val candidatesToFollow = findCommandWithTarget(commands, target, target, redanGrenat)
      val jumps = candidatesToFollow.map {
        case (("nop", _), ind) if infiniteLoop(ind) =>
          ind
        case (("jmp", _), ind) =>
          grenaBak(
            commands,
            infiniteLoop,
            newVisitedPositions,
            ind,
            redanGrenat,
            changeIndex
          )
        case _ => -1 //should never happen
      }
      val steps = if (target > 0) {
        commands(target-1) match {
          case ("nop", _) =>
            grenaBak(
              commands,
              infiniteLoop,
              newVisitedPositions,
              target = target - 1,
              redanGrenat = redanGrenat,
              changeIndex
            )
          case ("acc", _) =>
            grenaBak(
              commands,
              infiniteLoop,
              newVisitedPositions,
              target = target - 1,
              redanGrenat,
              changeIndex
            )
          case ("jmp", 1) =>
            grenaBak(
              commands,
              infiniteLoop,
              newVisitedPositions,
              target = target - 1,
              redanGrenat,
              changeIndex
            )
          case ("jmp", num) if !redanGrenat && infiniteLoop(target-1) =>
            target-1
          case _ => -1 //should never happen
        }
      } else -1
      jumps.appended(steps).maxOption.getOrElse(-1)
    }
  }

  def findCommandWithTarget(commands: Seq[(String, Int)], targetLow: Int, targetHigh: Int, redanGrenat: Boolean) = {
    commands.zipWithIndex.filter {
      case (("nop", num), ind) if !redanGrenat && num + ind >= targetLow && num + ind <= targetHigh => true
      case (("jmp", num), ind) if num + ind >= targetLow && num + ind <= targetHigh => true
      case _ => false
    }
  }

  def parseLine(line: String): (String, Int) = {
    val bags = line.split(" ")
    (bags.head, Integer.parseInt(bags.last))
  }


}

package aoc2020

import scala.collection.mutable

object AocHelpers {

  def readDataGroupsSeparatedByBlankLines(fileName: String): Seq[Seq[String]] = {

    val bufferedSource = io.Source.fromFile(fileName)
    var rows: Seq[Seq[String]] = Seq(Seq())
    for (line <- bufferedSource.getLines) {
      if (line.isBlank)
        rows = rows.appended(Seq())
      else {
        rows = rows.dropRight(1) :+ rows.last.appended(line)
      }
    }
    rows
  }

  def readLines(fileName: String): Seq[String] = {

    val bufferedSource = io.Source.fromFile(fileName)
    var rows: Seq[String] = Seq()
    for (line <- bufferedSource.getLines) {
      rows = rows.appended(line)
    }
    rows
  }
}


class AssRunner(prog: Seq[(String, Int)]) {
  private var currentPosition = 0
  private val program: mutable.Seq[(String, Int)] = mutable.Seq(prog: _*)
  private var visitedPositions: mutable.Seq[Boolean] = program.map(_ => false)
  private var steps = 0
  private var accumulator = 0

  def getVisitedPositions: mutable.Seq[Boolean] = visitedPositions

  def run(): AssRunner.State = {
    var state: AssRunner.State = AssRunner.Running
    while (state == AssRunner.Running) {
      state = step()
    }
    //println(s"Done: $program")
    state
  }

  def step(): AssRunner.State = {
    steps = steps + 1
    if (currentPosition > program.length)
      AssRunner.Halted(accumulator, 1)
    else if (currentPosition == program.length)
      AssRunner.Halted(accumulator, 0)
    else if (visitedPositions(currentPosition))
      AssRunner.Halted(accumulator, 1)
    else {
      visitedPositions(currentPosition) = true
      program(currentPosition) match {
        case ("acc", num) =>
          accumulator = accumulator + num
          currentPosition = currentPosition + 1
          AssRunner.Running
        case ("jmp", num) =>
          currentPosition = currentPosition + num
          AssRunner.Running
        case ("nop", _) =>
          currentPosition = currentPosition + 1
          AssRunner.Running
        case cmd => throw AssRunner.AssException(s"Unknown command: $cmd")
      }
    }
  }
}

object AssRunner {

  sealed trait State

  case object Running extends State

  case class Halted(result: Int, exitCode: Int) extends State

  case class AssException(msg: String) extends RuntimeException(msg: String)

}

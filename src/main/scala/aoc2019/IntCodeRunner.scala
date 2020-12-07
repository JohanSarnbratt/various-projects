package aoc2019

import scala.collection.mutable

class IntCodeRunner(prog: Seq[Int]) {
  private var currentPosition = 0
  private var program: mutable.Seq[Int] = mutable.Seq(prog: _*)
  private var steps = 0
  def run() = {
    while (step() == IntCodeRunner.Running) {}
    println(s"Done: $program")
    program
  }
  def step(): IntCodeRunner.State = {
    steps = steps + 1
    val res = program(currentPosition) match {
      case 1 =>
        program(program(currentPosition+3)) = program(program(currentPosition+1)) + program(program(currentPosition+2))
        currentPosition = currentPosition+4
        IntCodeRunner.Running
      case 2 =>
        program(program(currentPosition+3)) = program(program(currentPosition+1)) * program(program(currentPosition+2))
        currentPosition = currentPosition+4
        IntCodeRunner.Running
      case 99 =>
        IntCodeRunner.Halted
      case cmd => throw IntCodeRunner.IntCodeException(s"Unknown intcode command: $cmd")
    }
    println(program)
    res
  }
}

object IntCodeRunner {
  sealed trait State

  case object Running extends State

  case object Halted extends State

  case class IntCodeException(msg: String) extends RuntimeException(msg: String)
}
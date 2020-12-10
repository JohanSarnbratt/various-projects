package aoc2019

import scala.collection.mutable

class IntCodeRunner(prog: Seq[Int], input: Seq[Int] = Seq.empty) {
  private var inputIndex = 0
  private var currentPosition = 0
  private var program: mutable.Seq[Int] = mutable.Seq(prog: _*)
  private var output: Seq[Int] = Seq.empty[Int]
  private var steps = 0
  def run() = {
    while (step() == IntCodeRunner.Running) {}
    //println(s"Done: $program")
    program
  }
  def getOutput: Seq[Int] = output
  def step(): IntCodeRunner.State = {
    steps = steps + 1
    val (opCode, params) = parseOp(program(currentPosition))
    def getVal(offset: Int) = {
      params(offset-1) match {
        case 0 => program(program(currentPosition+offset))
        case 1 => program(currentPosition+offset)
      }
    }
    val res = opCode match {
      case 1 => // add
        program(program(currentPosition+3)) = getVal(1) + getVal(2)
        currentPosition = currentPosition+4
        IntCodeRunner.Running
      case 2 => // multiply
        program(program(currentPosition+3)) = getVal(1) * getVal(2)
        currentPosition = currentPosition+4
        IntCodeRunner.Running
      case 3 => // input
        program(program(currentPosition+1)) = input(inputIndex)
        currentPosition = currentPosition+2
        inputIndex = inputIndex + 1
        IntCodeRunner.Running
      case 4 => //output
        output = output.appended(getVal(1))
        currentPosition = currentPosition+2
        IntCodeRunner.Running
      case 5 => // jump if true
        if (getVal(1) != 0)
          currentPosition = getVal(2)
        else
          currentPosition = currentPosition+3
        IntCodeRunner.Running
      case 6 => // jump if false
        if (getVal(1) == 0)
          currentPosition = getVal(2)
        else
          currentPosition = currentPosition+3
        IntCodeRunner.Running
      case 7 => // less than
        if (getVal(1) < getVal(2))
          program(program(currentPosition+3)) = 1
        else
          program(program(currentPosition+3)) = 0
        currentPosition = currentPosition+4
        IntCodeRunner.Running
      case 8 => // equals
        if (getVal(1) == getVal(2))
          program(program(currentPosition+3)) = 1
        else
          program(program(currentPosition+3)) = 0
        currentPosition = currentPosition+4
        IntCodeRunner.Running
      case 99 =>
        IntCodeRunner.Halted
      case cmd => throw IntCodeRunner.IntCodeException(s"Unknown intcode command: $cmd program: $program current: $currentPosition")
    }
    //println(program)
    res
  }
  def parseOp(op: Int): (Int, Seq[Int]) = {
    val opCode = op%100
    val params = Seq((op/100) % 10, (op/1000) % 10, (op/10000) % 10)
    (opCode, params)
  }
  def getVal(index: Int, parameterMode: Int) = {
    parameterMode match {
      case 0 => program(program(index))
      case 1 => program(index)
    }
  }
}

object IntCodeRunner {
  sealed trait State

  case object Running extends State

  case object Halted extends State

  case class IntCodeException(msg: String) extends RuntimeException(msg: String)
}
package aoc2019

import scala.collection.mutable

class IntCodeRunner(prog: Seq[Long]) {
  private var input: Seq[Long] = Seq.empty
  private var inputIndex = 0
  private var currentPosition = 0
  private var program: mutable.Seq[Long] = mutable.Seq(prog: _*)
  private var output: Seq[Long] = Seq.empty[Long]
  private var steps = 0
  def run(newInput: Seq[Long] = Seq.empty): IntCodeRunner.State = {
    input = input ++ newInput
    var status = step()
    while (status == IntCodeRunner.Running) { status = step() }
    status
  }
  def getOutput: Seq[Long] = output
  def getProgram: mutable.Seq[Long] = program
  def step(): IntCodeRunner.State = {
    steps = steps + 1
    val (opCode, params) = parseOp(program(currentPosition).toInt)
    def getVal(offset: Int): Long = {
      params(offset-1) match {
        case 0 => program(program(currentPosition+offset).toInt)
        case 1 => program(currentPosition+offset)
      }
    }
    def setVal(offset: Int, value: Long): Unit = {
      params(offset-1) match {
        case 0 => program(program(currentPosition+offset).toInt) = value
      }
    }
    val res = opCode match {
      case 1 => // add
        setVal(3, getVal(1) + getVal(2))
        currentPosition = currentPosition+4
        IntCodeRunner.Running
      case 2 => // multiply
        setVal(3, getVal(1) * getVal(2))
        currentPosition = currentPosition+4
        IntCodeRunner.Running
      case 3 => // input
        if (inputIndex < input.length) {
          setVal(1, input(inputIndex))
          currentPosition = currentPosition + 2
          inputIndex = inputIndex + 1
          IntCodeRunner.Running
        } else {
          IntCodeRunner.NeedInput
        }
      case 4 => //output
        output = output.appended(getVal(1))
        currentPosition = currentPosition+2
        IntCodeRunner.Running
      case 5 => // jump if true
        if (getVal(1) != 0)
          currentPosition = getVal(2).toInt
        else
          currentPosition = currentPosition+3
        IntCodeRunner.Running
      case 6 => // jump if false
        if (getVal(1) == 0)
          currentPosition = getVal(2).toInt
        else
          currentPosition = currentPosition+3
        IntCodeRunner.Running
      case 7 => // less than
        if (getVal(1) < getVal(2))
          setVal(offset = 3, value = 1)
        else
          setVal(offset = 3, value = 0)
        currentPosition = currentPosition+4
        IntCodeRunner.Running
      case 8 => // equals
        if (getVal(1) == getVal(2))
          setVal(offset = 3, value = 1)
        else
          setVal(offset = 3, value = 0)
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
}

object IntCodeRunner {
  sealed trait State

  case object Running extends State

  case object Halted extends State

  case object NeedInput extends State

  case class IntCodeException(msg: String) extends RuntimeException(msg: String)
}
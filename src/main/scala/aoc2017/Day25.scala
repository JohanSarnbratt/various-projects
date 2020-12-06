package aoc2017

import scala.collection.mutable

object Day25 {
  def run() = {
    val states = Map(
      'A' -> State(Rule(1, 1, 'B'), Rule(0, -1, 'C')),
      'B' -> State(Rule(1, -1, 'A'), Rule(1, 1, 'C')),
      'C' -> State(Rule(1, 1, 'A'), Rule(0, -1, 'D')),
      'D' -> State(Rule(1, -1, 'E'), Rule(1, -1, 'C')),
      'E' -> State(Rule(1, 1, 'F'), Rule(1, 1, 'A')),
      'F' -> State(Rule(1, 1, 'A'), Rule(1, 1, 'E'))
    )
    val tm = new TuringMachine(states)
    (1 to 12134527).foreach { _ => tm.step() }
    tm.print()
  }
}

/**
 *
 * @param write what to write: 0 or 1
 * @param direction 1 for right, -1 for left
 * @param nextState Next state
 */
case class Rule(write: Int, direction: Int, nextState: Char) {
  require(write == 0 || write == 1)
  require(direction == -1 || direction == 1)
}

case class State(onZero: Rule, onOne: Rule)

class TuringMachine(states: Map[Char, State]) {
  private var currentState = 'A'
  private val tapeSize = 1000000
  private var currentIndex = tapeSize/2
  private val tape: mutable.Seq[Int] = mutable.Seq.fill(tapeSize)(0)
  private var checkSum = 0
  private var steps = 0
  def step() = {
    val state = states(currentState)
    val oldVal = tape(currentIndex)
    val rule =
      if (oldVal == 0)
        state.onZero
      else
        state.onOne
    tape(currentIndex) = rule.write
    currentState = rule.nextState
    currentIndex = currentIndex + rule.direction
    checkSum = checkSum + rule.write - oldVal
    steps = steps + 1

    if (currentIndex < 0 || currentIndex >= tapeSize) {
      println(s"Reached index $currentIndex after $steps steps")
      throw new RuntimeException(s"Reached index $currentIndex after $steps steps")
    }
  }
  def print() = {
    //println(tape)
    println(s"currentIndex: $currentIndex")
    println(s"checkSum: $checkSum")
    println(s"steps: $steps")
  }
}
//Reached index 1000 after 834 steps
//Reached index 10000 after 8334 steps
//Reached index 100000 after 166666 steps
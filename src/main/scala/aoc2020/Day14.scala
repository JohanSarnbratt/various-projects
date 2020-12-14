package aoc2020

import scala.collection.mutable

object Day14 {

  def run() = { //part 1
    val data = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data14")
    val program: Seq[(String, String)] = data.map(_.split(" = ")).map(arr => (arr.head, arr(1)))
    val masker = new Masker(program)
    masker.run()
    println(masker.getMemory.take(10))
    println(masker.getMemory.sum)
    val data2 = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data14")
    val program2: Seq[(String, String)] = data2.map(_.split(" = ")).map(arr => (arr.head, arr(1)))
    val masker2 = new Masker2(program2)
    masker2.run()
    println(masker2.getMemory.take(10))
    println(masker2.getMemory.values.sum)
  }
}

class Masker2(prog: Seq[(String, String)]) {
  private var currentPosition = 0
  private val program: mutable.Seq[(String, String)] = mutable.Seq(prog: _*)

  private val visitedPositions: mutable.Seq[Boolean] = program.map(_ => false)
  private var steps = 0
  private var mask = "X"*36
  private var memory: mutable.Map[String, Long] = mutable.Map.empty[String, Long].withDefaultValue(0L)

  def getVisitedPositions: mutable.Seq[Boolean] = visitedPositions
  def getMemory: Map[String, Long] = memory.toMap

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
      AssRunner.Halted(0, 1)
    else if (currentPosition == program.length)
      AssRunner.Halted(0, 0)
    else if (visitedPositions(currentPosition))
      AssRunner.Halted(0, 2)
    else {
      visitedPositions(currentPosition) = true
      program(currentPosition) match {
        case ("mask", newMask) =>
          mask = newMask
          currentPosition = currentPosition + 1
          AssRunner.Running
        case (mem, num) =>
          val address: Int = mem.split('[')(1).split(']').head.toInt
          val addresses = getAddresses(address)
          println(addresses)
          println(num)
          memory = memory.subtractAll(addresses)
          memory = memory.addAll(addresses.map((_, num.toLong)))
          currentPosition = currentPosition + 1
          AssRunner.Running
        case cmd => throw AssRunner.AssException(s"Unknown command: $cmd")
      }
    }
  }
  def getAddresses(address: Int): Seq[String] = {
    val binaruNum = address.toLong.toBinaryString
    val fullBinary: String = "0"*(36 - binaruNum.length) + binaruNum
    val numOfAddresses = Math.pow(2, mask.count('X' == _)).toInt
    println(s"numOfAddresses: $numOfAddresses")
    (0 until numOfAddresses).map {
      ind =>
        println(ind.toBinaryString)
        var i = ind
        val res = fullBinary.zip(mask).map {
          case (_, 'X') =>
            val r = (i%2).toString.head
            i = i/2
            r

          case (a, '0') =>
            a
          case (_, '1') =>
            '1'
        }.mkString
        println(s"res.length: ${res.length}")
        res
    }
  }
}
class Masker(prog: Seq[(String, String)]) {
  private var currentPosition = 0
  private val program: mutable.Seq[(String, String)] = mutable.Seq(prog: _*)

  private val visitedPositions: mutable.Seq[Boolean] = program.map(_ => false)
  private var steps = 0
  private var mask = "X"*36
  private val memory: mutable.Seq[Long] = mutable.Seq.fill(100000)(0L)

  def getVisitedPositions: mutable.Seq[Boolean] = visitedPositions
  def getMemory: Seq[Long] = memory.toSeq

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
      AssRunner.Halted(0, 1)
    else if (currentPosition == program.length)
      AssRunner.Halted(0, 0)
    else if (visitedPositions(currentPosition))
      AssRunner.Halted(0, 2)
    else {
      visitedPositions(currentPosition) = true
      program(currentPosition) match {
        case ("mask", newMask) =>
          mask = newMask
          currentPosition = currentPosition + 1
          AssRunner.Running
        case (mem, num) =>
          val address = mem.split('[')(1).split(']').head.toInt
          val binaruNum = num.toLong.toBinaryString
          val fullBinary = "0"*(36 - binaruNum.length) + binaruNum
          val writeString: String = mask.zip(fullBinary).map {
            case ('X', digit) => digit.toString
            case (maskDig, _) => maskDig.toString
          }.reduce(_.concat(_))

          val writeVal = java.lang.Long.parseLong(writeString, 2)
          memory(address) = writeVal
          currentPosition = currentPosition + 1
          AssRunner.Running
        case cmd => throw AssRunner.AssException(s"Unknown command: $cmd")
      }
    }
  }
}
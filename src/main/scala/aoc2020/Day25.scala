package aoc2020

import scala.collection.mutable

object Day25 {
  def run(): Unit = {
    println("Test: ")
    println(part1(5764801L, 17807724L))
    println(part1(8252394L, 6269621L))
  }

  val Divider = 20201227L
  val Subject: Long = 7L
  def part1(cardPubKey: Long, doorPubKey: Long) = {
    var loops = 0
    var keyNum: Long = 1L
    var cardLoops = 0
    var doorLoops = 0
    while (cardLoops == 0 || doorLoops == 0) {
      loops = loops+1
      keyNum = (keyNum*Subject)%Divider
      if (keyNum == cardPubKey)
        cardLoops = loops
      if (keyNum == doorPubKey)
        doorLoops = loops
    }
    println(s"$cardLoops $doorLoops")
    d(cardPubKey, doorLoops)
  }
  def d(key: Long, loops: Int) = {
    var out: Long = 1L
    println("")
    (0 until loops).foreach {
      _ =>
        out = (out*key)%Divider
        //println(out)
    }
    out
  }
}

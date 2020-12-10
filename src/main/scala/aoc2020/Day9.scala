package aoc2020

object Day9 {

  def run() = { //part 1
    val data = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data9")
    val numbers = data.map(_.toLong)
    val florp = (0 until (numbers.length - 25)).flatMap(ind => checkSum(numbers.slice(ind, ind + 26)))
    findSum(florp.head, numbers)
  }

  def checkSum(seq: Seq[Long]) = {
    val last = seq.last
    val l = seq.length
    val correctSum = (0 until l - 2).exists(ind => {
      (ind+1 until l - 1).exists(ind2 => seq(ind) + seq(ind2) == last)
    }
    )
    if (!correctSum) {
      println(s"Didn't satisfy rule: $last")
      Some(last)
    } else None

  }

  def findSum(florp: Long, seq: Seq[Long]) = {
    var lowInd = 0
    var highInd = 1
    var sum: Long = seq(0) + seq(1)
    while (sum != florp) {

      while (sum < florp) {
        highInd = highInd+1
        sum = sum + seq(highInd)
      }
      while (sum > florp) {
        sum = sum - seq(lowInd)
        lowInd = lowInd+1
      }
    }
    val slice = seq.slice(lowInd, highInd+1)
    println(s"$lowInd $highInd")
    println(s"${slice.min} ${slice.max}")
    println(s"${slice.min + slice.max}")
  }

}

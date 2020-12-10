package aoc2020

object Day10 {

  def run() = { //part 1
    val data = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data10")
    val numbers = data.map(_.toInt).sorted
    println(numbers)
    val florp = numbers.foldLeft((0,0,0))((xyz, w) => w-xyz._3 match {
      case 1 => (xyz._1+1, xyz._2, w)
      case 3 => (xyz._1, xyz._2+1, w)
      case _ => (xyz._1, xyz._2, w)
    })
    println(florp)
    println(florp._1 * (florp._2+1))
    //println(findArrangements(Seq(0) ++ numbers ++ Seq(numbers.max+3), 1))
    println(divAndCon(Seq(0) ++ numbers ++ Seq(numbers.max+3)))

  }

  def divAndCon(numbers: Seq[Int]): Long = {
    var startInd = 0
    var endInd = 0
    var res = 1L
    while(startInd < numbers.length - 1) {
      while (endInd < numbers.length - 2 && numbers(endInd+1)-numbers(endInd) != 3)
        endInd = endInd+1 //
      println(s"$startInd $endInd")
      println(s"${numbers.slice(startInd, endInd+1)}")
      val r = findArrangements(numbers.slice(startInd, endInd+1), 1)
      println(s"r: $r")
      res = res * r
      startInd = endInd+1
      endInd = startInd
    }
    res
  }

  def findArrangements(numbers: Seq[Int], index: Int): Long = {
    if (index >= numbers.length - 1) {
      //don't remove last element
      return 1L
    } else {
      findArrangements(numbers, index+1) + {
        if ( numbers(index + 1) - numbers(index - 1) <= 3)
          findArrangements(dropKth(numbers, index), index)
        else
          0
      }
    }
  }

  def dropKth (seq: Seq[Int], ind: Int) = seq.take(ind) ++ seq.drop(ind+1)


}

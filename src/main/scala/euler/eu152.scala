package euler

import scala.collection.mutable

object eu152 {

  val lcm80: BigInt = BigInt("32433859254793982911622772305630400")
  val lcm80sq: BigInt = lcm80*lcm80
  val target = lcm80sq/2
  def run() = {
    val max = 38
    val minVal = f(max)
    val restSums = (2 to max).map(m => restSum(m, max))
    //println(lcm80sq)
    //restSums.map(println)
    val fs = (2 to max).map(f)
    val calls = mutable.Seq.fill(max+5)(0)
    def recur(sumLeft: BigInt, numsSoFar: List[Int], n: Int): Int = {
      calls(n) = calls(n) + 1
      if (sumLeft == 0) {
        println(s"Found solution $numsSoFar")
        1
      } else if (n > max || sumLeft - restSums(n-2) > 0) {
        0
      } else if (sumLeft >= minVal) {
        val res = recur(sumLeft, numsSoFar, n + 1) +
          recur(sumLeft - fs(n-2), n :: numsSoFar, n + 1)
        res
      } else {
        0
      }
    }

    println("Starting...")
    val t = System.nanoTime()
    val noOfSolutions = recur(target, Nil, 2)
    println((System.nanoTime()-t)/1000000000.0)
    println(s"noOfSolutions: $noOfSolutions")
    println(s"calls: ${calls.zipWithIndex}")
    println(s"sum calls: ${calls.sum}")
  }
  def abs(int: BigInt) = if (int < 0) -int else int

  def f(n: Int) = lcm80sq / (n * n)

  def restSum(n: Int, max: Int): BigInt = (n to max).map(f).sum
}

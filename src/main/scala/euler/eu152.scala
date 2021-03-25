package euler

object eu152 {

  val lcm80: BigInt = BigInt("32433859254793982911622772305630400")
  val lcm80sq: BigInt = lcm80*lcm80
  val target = lcm80sq/2
  def run() = {
    val max = 35
    val restSums = (2 to max).map(m => restSum(m, max))
    //println(lcm80sq)
    //restSums.map(println)
    val fs = (2 to max).map(f)
    var record = lcm80sq
    def recur(sumSoFar: BigInt, numsSoFar: Seq[Int], n: Int): Unit = {
      if (abs(sumSoFar - target) < record) {
        record = abs(sumSoFar - target)
        println(s"New record $record")
        println(s"with $numsSoFar")
      }
      if (sumSoFar == target) {
        println(s"Found solution $numsSoFar")
      } else if (n > max || sumSoFar + restSums(n-2) < target) {
      } else if (sumSoFar < target) {
        recur(sumSoFar, numsSoFar, n + 1)
        recur(sumSoFar + fs(n-2), numsSoFar.appended(n), n + 1)
      }
    }

    println("Starting...")
    val t = System.nanoTime()
    recur(0, Seq(), 2)
    println((System.nanoTime()-t)/1000000000.0)
  }
  def abs(int: BigInt) = if (int < 0) -int else int

  def f(n: Int) = lcm80sq / (n * n)

  def restSum(n: Int, max: Int): BigInt = (n to max).map(f).sum
}

package euler


object eu700 {
  //sum for eulercoins >=4765 is 1517926517696910
  val mod = 4503599627370517L
  val gen = 1504170715041707L
  def run2(startAt: Long) = {
    val genInv = findMultiplicateInverse(gen)
    //val step = multiply(genInv, startAt, mod)
    val list = (startAt - 1 to 1 by -1)
      .map(s => (s, multiply(genInv, s, mod)))
      .filter(s => s._1 < startAt)
      .sortBy(_._2)
    val decList = takeDecreasing(list.toList)
    println(decList)
    decList.map(_._1)
      .sum
  }
  def takeDecreasing(list: List[(Long, Long)]): List[(Long, Long)] = list match {
    case (a1,a2) :: (b1,b2) :: l if a1 > b1 => (a1,a2) :: takeDecreasing((b1,b2) :: l)
    case (a1,a2) :: (b1,b2) :: l => takeDecreasing((a1,a2) :: l)
    case (a1,a2) :: Nil => (a1,a2) :: Nil
    case Nil => Nil
  }

  def findMultiplicateInverse(mul: Long, exponent: Long = mod-2): Long = {
    val res = exponent match {
      case 0 => 1L
      case e if e%2 != 0 =>
        val p = findMultiplicateInverse(mul, e/2)
        multiply(multiply(p,p,mod),mul,mod)
      case e =>
        val p = findMultiplicateInverse(mul, e/2)
        multiply(p,p,mod)
    }
    //println(s"$mul ^ $exponent = $res")
    res
  }

  def run() = {
    var min = 4503599627370517L
    var sum = 0L
    var current = gen
    var step = 1L
    var lastRecordAtStep = 0L
    while (current > 1000000) {
      if (current < min) {
          val steps: Long = step - lastRecordAtStep
          println(s"Found new min: ${current}\t diff: ${min - current}\tsteps since: ${steps}")
          min = current
          sum = sum + min
          println(s"sum = $sum\ttotal steps: $step")
          lastRecordAtStep = step
          current = (current + multiply(steps, gen, mod)) % mod
          step = step + steps
      } else {
        current = (current + gen) % mod
        step = step + 1
      }
    }
    println(s"Found sum: ${sum}")
    val rest = run2(min)
    println(s"Found result: ${(sum+rest)}")

  }
  def multiply(aa: Long, bb: Long, mod: Long) = {
    var res = 0L
    var a = aa % mod
    var b = bb
    while(b > 0) {
      if (b %2 == 1)
        res = (res + a) % mod
      a = (a * 2) % mod
      b = b / 2
    }
    res
  }
}


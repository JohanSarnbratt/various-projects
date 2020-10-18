package euler

import euler.helpers.RichLong.PowerLong

object eu141 {
  var setOfNums: Set[Long] = Set.empty
  def run() = {
    setOfNums = Set.empty
    genProgressiveTriples()
    println("nums: " ++ setOfNums.toString)
    println("sum: " ++ setOfNums.sum.toString)
  }
  val limit: Long = 1000L*1000L*1000L*1000L
  def genProgressiveTriples(): Unit = {
    def z(x: Long, y: Long): Long = y*y/x
    (1L to 1000L*1000L).foreach { x: Long =>
      var y = x+1
      while(z(x, y) < limit) {
        if(properTriplet(x, y, z(x,y))) {
          //println(s"checking $d $q $r")
          checkForSquares(x, y, z(x,y))
        }
        y = y+1
      }
    }
  }
  def properTriplet(small: Long, med: Long, big: Long): Boolean = {
    if (small > med || med > big) {
      println(s"$small < $med < $big is false")
      throw new RuntimeException("")
    }
    small*big == med*med
  }
  def checkForSquares(small: Long, med: Long, big: Long): Boolean = {
    //isSquare(small, med, big)
    isSquare(small, big, med)
    isSquare(big, med, small)
  }
  def isSquare(d: Long, q: Long, r: Long): Boolean = {
    val n = d*q+r
    val yes = n < limit && scala.math.sqrt(n).intValue**2 == n
    if (yes) {
      setOfNums += n
      println(s"$n = $d*$q+$r")
    }
    yes
  }
  def isProgressive(n: Long): Boolean = {
    (1L to n).exists(d => isProgressive(n,d))
  }
  def isProgressive(n: Long, d: Long): Boolean = {
    val q = n/d
    val r = n%d
    val ordered = Seq(d,r,q).sorted
    val yes = ordered(0)*ordered(2) == ordered(1)* ordered(1)
    if (yes) println(s"$n = $d*$q+$r")
    yes
  }
}

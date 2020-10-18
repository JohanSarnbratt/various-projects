package euler

import euler.RichLong.PowerLong

import scala.math.pow

object eu719 {
  def run() = {
    val snumbers = (2L to 1000000L).filter(_%9 < 2).filter(isRootOfSNumber).map(_**2)
    println("nums: " ++ snumbers.toString)
    println("sum: " ++ snumbers.sum.toString)
  }

  def isRootOfSNumber(n: Long) = {
    val sCandidate = n*n
    // find progression
    val found = findRepresentation(sCandidate, n)
    if (found) println(" , " ++ sCandidate.toString ++ " = " ++ n.toString ++ "^2")
    found
  }
  def findRepresentation(sCandidate: Long, n: Long): Boolean = {
    (sCandidate: Long, n: Long) match {
      case (i, i1) if i == i1 =>
        print(i1)
        true
      case (l, l1) if l < l1 => false
      case _ =>
        val nextSteps = lastKDigs(sCandidate)
        nextSteps.exists {
          parts =>
            val found = findRepresentation(parts._1, n-parts._2)
            if (found) print("+"++parts._2.toString)
            found
        }
    }
  }
  def lastKDigs(s: Long): Seq[(Long, Long)] = {
    val numberOfDigits = s.toString.length
    (1 until numberOfDigits).map(n => (s/(10**n), s%(10**n)))
  }

}
object RichLong {
  implicit class PowerLong(i: Long) {
    def ** (b: Long): Long = pow(i, b).longValue
  }
}
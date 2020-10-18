package euler

import scala.math.pow

object helpers {

  object RichLong {
    implicit class PowerLong(i: Long) {
      def ** (b: Long): Long = pow(i, b).longValue
    }
  }

  object RichInt {
    implicit class PowerInt(i: Int) {
      def ** (b: Int): Int = pow(i, b).intValue
    }
  }
}

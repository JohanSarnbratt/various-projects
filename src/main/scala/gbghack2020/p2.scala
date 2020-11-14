package gbghack2020

object p2 {
  def run() = {
    val word: String = "iQKgKGeKiOEOaCggo_eI"
    val bs: Seq[Int] = word.map(_.toInt)
    printer(bs)
    println(2047 << 1)
    println(rotateLeft(255))
    val x = 128+32+8+2
    println(x)
    println(rotateLeft(x))
    println(rotateLeft(rotateLeft(x)))
    println(rotateRight(x))
    println(rotateRight(rotateRight(x)))

   /* printer(bs.map(b => karusell(b, 1)))
    printer(bs.map(b => karusell(b, 2)))
    printer(bs.map(b => karusell(b, 3)))
    printer(bs.map(b => karusell(b, 4)))
    printer(bs.map(b => karusell(b, 5)))
    printer(bs.map(b => karusell(b, 6)))
    printer(bs.map(b => karusell(b, 7)))
    printer(bs.map(b => karusell(b, 8)))*/

    /*val entire: String = bs
      .map((i: Int) => i.toBinaryString)
      .map((str: String) => ("0"*(8-str.length))+str)
      .mkString
    println(s"entire: $entire")
    strinter(entire)
    (1 to (16)).foreach(index => strinter(stringusell(entire, index)))*/
  }

  def strinter(string: String): Unit = {
    if (string.isEmpty) {
      println()
    } else {
      print(Integer.parseInt(string.take(8), 2).toChar)
      strinter(string.drop(8))
    }
  }

  def printer(bs: Seq[Int]) = {
    //println(bs.map(_.toBinaryString))
    println(bs.map(_.toChar).mkString)
  }

  val defaultBase = 128

  def karusell(int: Int, varv: Int) = {
    var res = int
    (1 to varv).foreach(_ => res = rotateLeft(res))

     res
  }
  def stringusell(int: String, varv: Int) = {
    var res = int
    (1 to varv).foreach(_ => res = rotateLeft(res))
    res
  }

  def rotateLeft(string: String) = {
    string.tail+string.head
  }

  def rotateLeft(int: Int, base: Int = defaultBase) = {
    val shifted = int << 1
    shifted % base + shifted / base
  }
  def rotateRight(int: Int, base: Int = defaultBase) = {
    val shifted = int >> 1
    shifted + (base/2)*(int%2)
  }
}

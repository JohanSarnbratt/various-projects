package aoc2020

object Day2 {

  def run() = { //part 2
    val data = readData()
    val l = data.length
    println(l)
    println(data.head)
    val res = data.count(
      pwob => {
        // part 1
        // val x = pwob.password.count(_ == pwob.chr)
        // x >= pwob.min && x <= pwob.max
        //part 2
        val res = (pwob.password.charAt(pwob.min-1) == pwob.chr) ^ (pwob.password.charAt(pwob.max-1) == pwob.chr)
        println(pwob)
        println(res)
        res
      }
    )
    println(res)
  }

  case class Pwobject(min: Int, max: Int, chr: Char, password: String)

  def readData() = {

    val bufferedSource = io.Source.fromFile("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data2")
    var rows: Seq[Pwobject] = Seq.empty[Pwobject]
    for (line <- bufferedSource.getLines) {
      val line2 = line.split('-')
      val min = Integer.parseInt(line2.head)
      //println(line2.mkString("Array(", ", ", ")"))
      val line3 = line2.tail.head.split(" ")
      val max = Integer.parseInt(line3.head)
      val chr = line3(1).head
      val pw = line3(2)
      rows = rows.appended(Pwobject(min, max, chr, pw))
    }
    rows
  }
}

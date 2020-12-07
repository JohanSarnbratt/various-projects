package aoc2020

object Day5 {

  def run() = { //part 2
    val data = readData()
    val l = data.length
    println(l)
    println(data.head)
    val res = data.map(
      seatString => {
        val bin = seatString
          .replace("F","0")
          .replace("B","1")
          .replace("L","0")
          .replace("R","1")
        Integer.parseInt(bin, 2)
      }
    )
    //part 1
    println(s"Part 1: ${res.max}")
    //part 2
    println(s"Part 2: ${findMissingElement(res.sorted)}")
  }

  def findMissingElement(list: Seq[Int]): Int = list match {
    case x :: y :: _  if y-x != 1 =>
      x + 1
    case _ :: tail =>  findMissingElement(tail)
    case _ => throw new RuntimeException("Did not find the missing element")
  }

  case class Pwobject(min: Int, max: Int, chr: Char, password: String)

  def readData() = {

    val bufferedSource = io.Source.fromFile("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data5")
    var rows: Seq[String] = Seq.empty
    for (line <- bufferedSource.getLines) {
      rows = rows.appended(line)
    }
    rows
  }
}

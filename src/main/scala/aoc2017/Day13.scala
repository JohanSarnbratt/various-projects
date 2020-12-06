package aoc2017

object Day13 {
  def run() = {
    val data: Seq[String] = aoc2020.AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2017/data13")
    val formattedData: Seq[(Int, Int)] = data.map(line => {
      val x = line.split(": ")
      (Integer.parseInt(x.head), Integer.parseInt(x(1)) * 2 - 2)
    })
    println(formattedData)
    var record = 4321765
    (0 until 41930678).foreach {
      offset =>
        val res = getHits(offset, formattedData)
        if (res < record) {
          println(s"offset $offset res: $res")
          record = res
        }
        if (res == 0)
          throw new RuntimeException("finished")
    }
  }

  def getHits(offset: Int, data: Seq[(Int, Int)]) = {
    data.map(
      xy => {
        //println((xy._1 + offset) % xy._2)
        if ((xy._1 + offset) % xy._2 == 0) {
          xy._1 * (xy._2 + 2) / 2+1
        } else {
          0
        }
      }
    ).sum
  }
}

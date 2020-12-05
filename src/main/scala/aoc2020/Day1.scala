package aoc2020

object Day1 {

  def runp1() = {
    val data = readData()
    val l = data.length
    println(l)
    (0 until l - 1).foreach(ind1 =>
      (ind1 + 1 until l).foreach(ind2 =>
        if (data(ind1) + data(ind2) == 2020)
          println(s"ind1: $ind1,ind2: $ind2,data(ind1): ${data(ind1)},data(ind2): ${data(ind2)},product: ${data(ind1) * data(ind2)}")
      )
    )
  }

  def run() = { //part 2
    val data = readData()
    val l = data.length
    println(l)
    (0 until l - 2).foreach(ind1 => {
      println(ind1)
      (ind1 + 1 until l - 1).foreach(ind2 =>
        (ind2 + 1 until l).foreach(ind3 =>
          if (data(ind1) + data(ind2) + data(ind3) == 2020)
            println(s"data(ind1): ${data(ind1)},data(ind2): ${data(ind2)},data(ind3): ${data(ind3)},product: ${data(ind1) * data(ind2) * data(ind3)}")
        )
      )
    }
    )
  }

  def readData() = {

    val bufferedSource = io.Source.fromFile("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data")
    var rows: Seq[Int] = Seq.empty
    for (line <- bufferedSource.getLines) {
      rows = rows.appended(Integer.parseInt(line))
    }
    rows
  }
}

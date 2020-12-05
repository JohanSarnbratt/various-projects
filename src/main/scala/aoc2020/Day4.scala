package aoc2020

object Day4 {

  def run() = { //part 2
    val data = readData()
    val l = data.length
    println(l)
    println(data.head)
    val res = data.map(
      seatString => {
        val bin = seatString
          .replace("F", "0")
          .replace("B", "1")
          .replace("L", "0")
          .replace("R", "1")
        Integer.parseInt(bin, 2)
      }
    )
    //part 1
    println(res.max)
    //part 2
    println(res.sorted)
  }

  case class Passport(
    byr: Option[String] = None,
    iyr: Option[String] = None,
    eyr: Option[String] = None,
    hgt: Option[String] = None,
    hcl: Option[String] = None,
    ecl: Option[String] = None,
    pid: Option[String] = None,
    cid: Option[String] = None
  ) {
    def valid: Boolean = Seq(byr,iyr,eyr, hgt, hcl, ecl, pid).forall(_.isDefined)
  }

  def readData() = {

    val bufferedSource = io.Source.fromFile("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data5")
    var rows: Seq[Passport] = Seq(Passport())
    for (line <- bufferedSource.getLines) {

      rows = rows.appended(line)
    }
    rows
  }
}

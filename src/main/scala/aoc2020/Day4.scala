package aoc2020

object Day4 {

  def run() = { //part 2
    val data = readData()
    val l = data.length
    println(l)
    println(data.head)
    println(data.count(_.valid))
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
    def set(key: String, value: String) = {
      key match {
        case "byr" => this.copy(byr = Some(value))
        case "iyr" => this.copy(iyr = Some(value))
        case "eyr" => this.copy(eyr = Some(value))
        case "hgt" => this.copy(hgt = Some(value))
        case "hcl" => this.copy(hcl = Some(value))
        case "ecl" => this.copy(ecl = Some(value))
        case "pid" => this.copy(pid = Some(value))
        case "cid" => this.copy(cid = Some(value))
      }
    }
    def valid: Boolean = {
      val allExist = Seq(byr, iyr, eyr, hgt, hcl, ecl, pid).forall(_.isDefined)
      if (!allExist)
        return allExist
      val yearsValid =
        Integer.parseInt(byr.get) >= 1920 && Integer.parseInt(byr.get) <= 2002 &&
        Integer.parseInt(iyr.get) >= 2010 && Integer.parseInt(iyr.get) <= 2020 &&
        Integer.parseInt(eyr.get) >= 2020 && Integer.parseInt(eyr.get) <= 2030
      val height = hgt.get
      val heightValid = height.takeRight(2) match {
        case "cm" =>
          val h = Integer.parseInt(height.dropRight(2))
          h >= 150 && h <= 193
        case "in" =>
          val h = Integer.parseInt(height.dropRight(2))
          h >= 59 && h <= 76
        case _ => false
      }
      val hclValid = hcl.get.head == '#' && hcl.get.tail.count("1234567890abcdef".contains(_)) == 6
      val eclValid = Seq("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(ecl.get)
      val pidValid = pid.get.length == 9 && pid.get.forall("1234567890".contains(_))
      yearsValid && heightValid && hclValid && eclValid && pidValid
    }
  }

  def readData() = {

    val bufferedSource = io.Source.fromFile("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data4")
    var rows: Seq[Passport] = Seq(Passport())
    for (line <- bufferedSource.getLines) {
      if (line.isBlank)
        rows = rows.appended(Passport())
      else {
        line.split(" ").foreach { kv =>
          val kvs = kv.split(":")
          rows = rows.dropRight(1) :+ rows.last.set(kvs.head, kvs.last)
        }
      }

    }
    rows
  }
}

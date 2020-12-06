package aoc2020

object AocHelpers {

  def readDataGroupsSeparatedByBlankLines(fileName: String) = {

    val bufferedSource = io.Source.fromFile(fileName)
    var rows: Seq[Seq[String]] = Seq(Seq())
    for (line <- bufferedSource.getLines) {
      if (line.isBlank)
        rows = rows.appended(Seq())
      else {
        rows = rows.dropRight(1) :+ rows.last.appended(line)
      }
    }
    rows
  }

  def readLines(fileName: String) = {

    val bufferedSource = io.Source.fromFile(fileName)
    var rows: Seq[String] = Seq()
    for (line <- bufferedSource.getLines) {
      rows = rows.appended(line)
    }
    rows
  }
}

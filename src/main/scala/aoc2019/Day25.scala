package aoc2019

object Day25 {

  case class Cord(x: Int,y: Int,z: Int,w: Int)
  object Cord {
    def apply(str: String): Cord = {
      val seq = str.split(",").map(_.trim).map(Integer.parseInt)
      Cord(seq(0),seq(1),seq(2),seq(3))
    }
  }
  def manhattanDistance(c1: Cord, c2: Cord): Int =
    Math.abs(c1.x-c2.x)+Math.abs(c1.y-c2.y)+Math.abs(c1.z-c2.z)+Math.abs(c1.w-c2.w)

  def readData(fileNo: Int = 0): Seq[Cord] = {

    val fileNames = Seq(
      "/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2019/data25",
      "/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2019/data25t1",
      "/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2019/data25t2",
      "/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2019/data25t3",
      "/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2019/data25t4"
    )
    val bufferedSource = io.Source.fromFile(fileNames(fileNo))
    var rows: Seq[Cord] = Seq.empty
    for (line <- bufferedSource.getLines) {
      rows = rows.appended(Cord(line))
    }
    rows
  }

  def run() = {
    val data = readData(0)
    var remainingCords = data
    var constellations: Seq[Seq[Cord]] = Seq.empty
    while (remainingCords.nonEmpty) {
      var newConstallation: Seq[Cord] = Seq(remainingCords.head)
      remainingCords = remainingCords.tail
      var ind = 0
      while(ind < newConstallation.length) {
        val newNeighbours: Seq[Cord] = findNeighbours(newConstallation(ind), remainingCords)
        newConstallation = newConstallation ++ newNeighbours
        remainingCords = remainingCords.diff(newNeighbours)
        ind = ind+1
      }
      constellations = constellations.appended(newConstallation)
    }
    println(constellations)
    println(s"Number of constellations: ${constellations.length}")
  }
  def findNeighbours(cord: Cord, otherCords: Seq[Cord]): Seq[Cord] = {
    otherCords.filter(potentialNeighbour => manhattanDistance(cord, potentialNeighbour) <= 3)
  }
}

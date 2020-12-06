package aoc2020

object Day6 {

  def run() = { //part 2
    val data = AocHelpers.readDataGroupsSeparatedByBlankLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data6")
    val l = data.length
    println(l)
    println("Part 1")
    println(data
      .map(_
        .flatten
        .distinct
        .length
      ).sum
    )
    println("Part 2")
    println(data.map(group => group
      .map(_.toSeq)
      .fold(group.head.toSeq)((l1, l2) => l1.intersect(l2))
      .length
    ).sum)
  }



}

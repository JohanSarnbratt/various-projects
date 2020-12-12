package aoc2020

object Day11 {

  def run() = { //part 1
    var data = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data11")
    println(data)
    var oldNoSeats = -1
    var newNoSeats = 0
    while(oldNoSeats != newNoSeats) {
      data = step(data)
      oldNoSeats = newNoSeats
      newNoSeats = countTotal(data)

    }
    println(s"Part 1: $newNoSeats")

     data = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data11")
    oldNoSeats = -1
     newNoSeats = 0
    while(oldNoSeats != newNoSeats) {
      data = step2(data)
      oldNoSeats = newNoSeats
      newNoSeats = countTotal(data)
      println(newNoSeats)
    }
    println(s"Part 2: $newNoSeats")

  }

  def step(seats: Seq[String]): Seq[String] = {
    var newSeats: Seq[String] = seats
    val width = seats.head.length
    val height = seats.length
    (0 until height).foreach{
      y =>
        (0 until width).foreach {
          x =>
            if (seats(y)(x) == 'L' && countNeighbours(seats, x, y) == 0) {
              newSeats = newSeats.updated(y, newSeats(y).updated(x, '#'))
            } else if (seats(y)(x) == '#' && countNeighbours(seats, x, y) >= 4) {
              newSeats = newSeats.updated(y, newSeats(y).updated(x, 'L'))
            }
        }
    }
    newSeats
  }
  def step2(seats: Seq[String]): Seq[String] = {
    var newSeats: Seq[String] = seats
    val width = seats.head.length
    val height = seats.length
    (0 until height).foreach {
      y =>
        (0 until width).foreach {
          x =>
            if (seats(y)(x) == 'L' && countNeighbours2(seats, x, y) == 0) {
              newSeats = newSeats.updated(y, newSeats(y).updated(x, '#'))
            } else if (seats(y)(x) == '#' && countNeighbours2(seats, x, y) >= 5) {
              newSeats = newSeats.updated(y, newSeats(y).updated(x, 'L'))
            }
        }
    }
    newSeats
  }
  def countNeighbours2(seats: Seq[String],x: Int,y: Int) = {
    val delta = Seq((-1, -1),(-1, 0),(-1, 1),(0, -1),(0, 1),(1, -1),(1, 0),(1, 1))
    val width = seats.head.length
    val height = seats.length
    //println(s"Counting for $x $y")
    delta.map {
      case (dx, dy) =>
        val distOption: Option[Int] = (1 until width).find {
          k =>
            k*dx+x >= 0 && k*dy+y >= 0 &&
              k*dx+x < width && k*dy+y < height &&
              (seats(k*dy+y)(k*dx+x) == '#' || seats(k*dy+y)(k*dx+x) == 'L')
        }
        distOption.map { k =>
          //println(s"Direction $dx $dy steps: $k")
          if (k * dx + x >= 0 && k * dy + y >= 0 && k * dx + x < width && k * dy + y < height && seats(k * dy + y)(k * dx + x) == '#') {
            1
          } else 0
        }.getOrElse(0)
    }.sum
  }
  def countNeighbours(seats: Seq[String],x: Int,y: Int) = {
    val delta = Seq(-1, 0, 1)
    val width = seats.head.length
    val height = seats.length
    delta.map {
      dx =>
        delta.map {
          dy =>
            if (dx == 0 && dy == 0) {
              0
            } else if (dx+x >= 0 && dy+y >= 0 && dx+x < width && dy+y < height && seats(dy+y)(dx+x) == '#') {
              1
            } else 0
        }.sum
    }.sum
  }
  def countTotal(seats: Seq[String]) = {
    seats.map(row => row.count(_ == '#')).sum
  }
}

package aoc2020

object Day12 {

  def run() = { //part 1
    var data = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data12")
    println(data)
    var dir = 0
    var x = 0
    var y = 0
    val coms: Seq[(Char, Int)] = data.map(s => (s.head, Integer.parseInt(s.tail)))
    coms.foreach {
      case ('N', dist) => y = y + dist
      case ('S', dist) => y = y - dist
      case ('W', dist) => x = x - dist
      case ('E', dist) => x = x + dist
      case ('F', dist) => dir match {
        case 90 => y = y + dist
        case 270 => y = y - dist
        case 180 => x = x - dist
        case 0 => x = x + dist
      }
      case ('R', dist) => dir = (dir - dist + 360) % 360
      case ('L', dist) => dir = (dir + dist + 360) % 360
    }
    println(s"$x $y $dir")
    println(Math.abs(x) + Math.abs(y))


    var shipx = 0
    var shipy = 0
    var wayx = 10
    var wayy = 1
    coms.foreach {
      case ('N', dist) => wayy = wayy + dist
      case ('S', dist) => wayy = wayy - dist
      case ('W', dist) => wayx = wayx - dist
      case ('E', dist) => wayx = wayx + dist
      case ('F', dist) =>

        shipy = shipy + dist * wayy

        shipx = shipx - dist * wayx


      case ('R', dist) =>
        println(dist / 90)
        println(0 until dist / 90)
        (0 until dist / 90).foreach { _ =>
          val newY = -wayx
          wayx = wayy
          wayy = newY
        }
      case ('L', dist) =>
        (0 until dist / 90).foreach { _ =>
          val newY = wayx
          wayx = -wayy
          wayy = newY
        }
    }
    println(s"$shipx $shipy $dir")
    println(Math.abs(shipx) + Math.abs(shipy))

  }
}

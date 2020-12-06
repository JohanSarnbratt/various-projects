package aoc2020

object Day3 {

  def runp1() = { //part 1
    val data = Day3.data
    val width = ".##.............##......#.....#".length+1 //add 1 for newline
    val height = data.length/width
    println(width*height)
    println(data.length)
    val res = (0 until height).map {
      y =>
        val x = (y*3)%(width-1)
        val ind = y*width+x
        //println(s"$x $y $ind ${data(ind)}")
        data(ind) match {
          case '#' => 1
          case _ => 0
        }
    }.sum
    println(res)
  }

  def run() = {
    println("")
    println("part 1")
    runp1()
    println("")
    println("part 2")
    runp2()
    println("")
    println("part 2 test data")
    runp2(true)
  }
  def runp2(test: Boolean = false) = { //part 2
    val (data, width) = if(test)
      (Day3.testData.filterNot(_ == '\n'), "..##.......".length)
    else
      (Day3.data.filterNot(_ == '\n'), ".##.............##......#.....#".length)
    val height = data.length/width+2
    println(width)
    println(height)
    val slopes = Seq((1,1),(3,1),(5,1),(7,1),(1,2))
    var lastCord = (0,0)
    val res = slopes.map {
      slope =>
        val trees = (0 until height).map {
          step =>
            val y = step*slope._2
            val x = (step*slope._1)%width
            val ind = y*width+x
            val treeHit = if (ind < data.length) {
              //println(s"$x $y $ind ${data(ind)}")
              lastCord = (x,y)
              data(ind) match {
                case '#' => 1
                case _ => 0
              }
            }
            else
              0
            treeHit
        }.sum
        println(lastCord)
        trees
    }
    println(res)
    println(res.map(_.toLong).product)
  }

  val testData = """..##.......
                   |#...#...#..
                   |.#....#..#.
                   |..#.#...#.#
                   |.#...##..#.
                   |..#.##.....
                   |.#.#.#....#
                   |.#........#
                   |#.##...#...
                   |#...##....#
                   |.#..#...#.#
                   |""".stripMargin

  val data = """.##.............##......#.....#
               |.#.#................#..........
               |...#..#.##..#.#......#.#.#.#..#
               |..#......#........#..#..#.#.#..
               |.......#....#..#..#.......#....
               |..#..#..##.#...#........#.###..
               |..#...#..#.....#.##....#.......
               |....#..###.#......#.##..#...##.
               |..#..........#.##.#...#........
               |#...#............##....#..##...
               |.......##....#.....##..#.#..#.#
               |..#..#..#...#....#....#....#...
               |.#...#.##........#####........#
               |..#..#......#.....##...#.......
               |....#......##....#.#....#.#..##
               |#.#.##....##..#.........#.###..
               |##..###..#..#.......###.......#
               |...#.#......#.........#....#...
               |.....#..........#.....##..#.#..
               |....##......#.#..#....#.#......
               |..#.....#..##.......##......#..
               |.........##.##.#..##...........
               |....#...#.....#....#.#.###....#
               |.##.#..#...##..#.......#......#
               |##..#..#..####..#.#..#...#.....
               |..###..#..#..#.###..#....#.##..
               |......#...###.#.#.....#........
               |.....#...#.#...#.......#.....#.
               |#........#..##...........#..#..
               |.#.##.##...#.....#.#....#..#...
               |..##.##....#.....#....#....##..
               |#.........##...##..#.....#..#..
               |........#.####....#...##.....#.
               |.#.#...#..#..#.#......##.....#.
               |..#..........##..#.#.#....#...#
               |#.......#...#...#.....#.##.#...
               |..#.....#..#.....####.#..#.#.##
               |...#.#..#...#.....#...#.#.#.#.#
               |.#..##....##.....#..#....###...
               |....#......##.#.#.....#......#.
               |..#.#...#......#.....##.......#
               |..#...###...#..#.#...#..#.....#
               |#..............#.....#....##..#
               |.#...#.......#.............#...
               |..###....#.##........#.#.......
               |#.##.......#..#............###.
               |#...#..##.#.#............######
               |..##..#....#.#.###...#..##.##..
               |.#...#.###.#....#...#....#...#.
               |#...#.......#...........#...##.
               |##.#......#####.............#..
               |....#..#......##..#..........#.
               |#.....#.....#.#.......#...#...#
               |....#...#.#..##........#.#..##.
               |..##.....##............#.#.###.
               |#.........#........#..###......
               |............#..................
               |.#.###...####...#.#..#......#..
               |...##.###.#....##.#..####..#.##
               |..#####.#.##...#.#...##.##....#
               |........##...#...#....##.....##
               |#...........###...#.#...##.#...
               |##......#...#.......###........
               |..#..#.##.#..###....#..#.###.#.
               |...#.#.#...#....#.##..#...#....
               |........#.##...##.#.....##...##
               |.#..........##..#..#..#.#...#..
               |#.#...#.##.#....#.##..#........
               |.#.#.#....##..##.####.....###..
               |..#....##....#..#..#..##.......
               |..##...#.......#...##.#....#...
               |...####.#.#...........#.#...#..
               |....####.....#.#.....#....##.##
               |..#.....#.#.............##....#
               |#.#....#####.##..####.#...#.#..
               |#.#....#.##.#.#.##..#.#...#....
               |......#.......#.......#.....#..
               |..#.....#....###.###..#..#..#..
               |#..#....##.###...##.....#......
               |..#..#...#..#.##..........#....
               |...#.#.#......#....#.##..#..##.
               |....##.#....#..#...##....###...
               |##.#.###.....#..#.#.#..#.....##
               |##..#.#........#...#..#.#......
               |....#.#.......##.#...........#.
               |.......##...#...#...#.....#....
               |.....#....#..#..###.#...#......
               |............#.#..#......#.#....
               |...##..#.##....##..##.#......#.
               |#.#.#......#.#.....#.#.#..#.#.#
               |...###..........#..#..#.##.....
               |......#......#......###..##....
               |#...##...#....#....#..#...#.#..
               |.......#..#......##.......#....
               |...#..#..#.....#.....#......##.
               |..#....###..........##..#...#..
               |..........#..#.#...#......#..#.
               |#...#....#.##.........#.#.#...#
               |.#.#.#...#.#...#.#..#..#....#.#
               |#.##....#..#.........#.##.##..#
               |..#.#..##.#....#.###.#...#....#
               |.#.......#...#.#.........#....#
               |.......#...#..........#.#..#...
               |...#.....##..#....#...###...#.#
               |#....##.##..........#.......#..
               |.##..##......#...#....#.##....#
               |....#.....##...##.#..#.........
               |...#.#..##.#.#..#.......#....#.
               |.#...#.#.#.#..#..#.##.......#..
               |..#..##...#.#..#.......#.#####.
               |.......#.#...........#....#.#..
               |.#.###..........#...#..#...#...
               |..#.#............##......##....
               |...##..#...###...##....#.#.##..
               |....#.##...#..#.#.#...........#
               |....#..#....##.....#.##.#.....#
               |..##......##.#.........#....#.#
               |###...#...#..#.#...#...........
               |.####.....#.....#.#....#..#....
               |.#....#..#..#..#...##.....###.#
               |#....##.#....#.##..#......##..#
               |.........#..#.#.....#.#....##.#
               |.....#.#...#....#.#...#....#..#
               |.#...#.#.....#.#......#.#......
               |#....##.......#.............#..
               |#..#...#........##..#..#......#
               |####..##.#..##..#.........####.
               |.#.##..#.#..#.#.#.##...#..#.#.#
               |.##.#..#............#......#...
               |###....#.##....#..#...#........
               |.....#..###..........#..#......
               |.##..##.....#..##....#...#.....
               |#...##...........#..#...###..#.
               |#..##....#...#.##.##....#......
               |...#...#..#.#.......##.......##
               |....#.....#..#...#.........#.#.
               |.#...##.#......#.#..#..#...##..
               |...##...##.##...##...#..#......
               |#..##.#..#..#............#...#.
               |..#.....#.........#........#.#.
               |#.#...#...#......#.#......#....
               |.##.....##.#.#....#.##...##.#..
               |.##..##.#.#....#.##............
               |.#.##.....##.#...#...###....#..
               |.#..............#.#....###.###.
               |....#..#...#.#.#..........#.#.#
               |.#.#..#.#.#...###..#...##......
               |.#.#.....###......#..........#.
               |........#.##...............#.#.
               |...#.#.#......#..#..##........#
               |..#.##......#.......#..#......#
               |...#...#...#...#..#..#........#
               |..#....#.....#....#..##........
               |.....#..#...##....#......##....
               |...##..##..#..........##....#.#
               |..#....#..#...#.##..#.....##...
               |###...#.#....#........#.......#
               |......#...#..#....###.........#
               |..###.#...#...#...#.#..###.#...
               |.##.#.......#.#..#..#......#.#.
               |...##...........#.#..#.#..#....
               |.......#.....####.#.....#...#.#
               |......##......##.#.#.#...#.#..#
               |..###.#####..#....#.#...#..##..
               |.....#..#......#........#......
               |#..##....#.#.##....#....#......
               |.#....#.##.####.##..#..#......#
               |#...##...#.#...##.#.##.##...#..
               |........#.#........#.#...#..#..
               |.#....###..#......#.##.###..#..
               |.#..#..#..#..#...#.#.........##
               |....#...#..#..............#....
               |........#...##.....#.......#...
               |..#......#.#..#.#..#.#.#...###.
               |....#...####....###....#......#
               |#...#.#...................#.##.
               |..#.#.###...#....##....##......
               |#..##..#.........#....#....####
               |.#....###...#.#...#......#...#.
               |......#..#.#..#.##...#.#.#..#..
               |.#...#.#.....#..##......#..#...
               |##.#..##.....##.#.#.......##...
               |.##.##.##..#...#.#.##.##.......
               |.#.#......#.....#...#.#..#.....
               |...#...........#..#.##..##..#..
               |.....#...##......#........#.#..
               |....#..............##.........#
               |..####.#....##..##......##.#.#.
               |.#.#..#...#..........#...###..#
               |....#.#.##.....###.#..#.##.....
               |.......##.#.#..#...#...##.#....
               |...#.##.....#....#...#...#.....
               |##.#.##..#..#.#.....#.#..#.....
               |..#..##........#....###..#..#..
               |..#.........##.....#......#...#
               |...##..........##......#.#.#...
               |#.....#..#..#......#......#....
               |.##...#..##....#.......##..#.#.
               |.#...##...##......####.##.#....
               |.....#.........#.#.####......#.
               |...#.....#.#.........##..#.....
               |##.#.###.#..#.#..#............#
               |...##..#.#....#....#..#........
               |..#.###......#...#.#.....#...#.
               |....##.##..#.....#...#.#.#....#
               |.......#.#..#...........#.#....
               |.#.#..##.#.......#.#..#.....###
               |...#.#.....#.#..#.##..#...#.#..
               |...#......##....#..............
               |......#...#....................
               |..#........#...##.##.#..#.#.#..
               |.#.###.#.##..##..#....##....#..
               |.....#..#.#...#.#..#..#.......#
               |..........#.##.#..##..####.....
               |............#.#......#.........
               |.#....#..#......#.....##.......
               |.....#........#.....##.#..#.#..
               |#..#.##...#.#.....#...#.####...
               |......#...#....#.##..##.#...#..
               |.#.#.##......##....#.#....#.##.
               |#.#.#....#.###....##....##.....
               |.##..#...#.##......#..#..#...##
               |...#....###....#...........#.#.
               |#.#.##.##...##....#....##.#...#
               |.#.#######.......#......#......
               |#......#...#.#.#.###....#.##..#
               |......##..#..##.........##.#.##
               |....##...#.#....##.....#.....#.
               |..#.#........##........#.#..##.
               |.....#..#.##.....#.....#..#.#..
               |.#..............#.......#......
               |.............#..#..........#...
               |.#..#.##....##.#..#...##.......
               |...........#..#.......#.#....#.
               |.#..#..........##...#.#.#...#..
               |......#....#..###....#......#..
               |.#...#...##..#..#..##..#..#.#..
               |#.#.........#....#..........##.
               |...##..#..##...#....##...##.##.
               |..#....#.####.........#.....##.
               |.....#.#...#.#...#.##.#...##..#
               |#...#.....#..#.......#...#..#..
               |..#.......#..##.#.....#....#...
               |.#.....#..##.#.....#...#.#...#.
               |.....#.##..........##....#...#.
               |...#....#...#........##...#...#
               |....##...#....#..........#.....
               |...#....##..#..####..##.#...#.#
               |#...###.###..#....##.#.........
               |.#.......#......#.........#....
               |..#..##..#.........##..........
               |#......#.#.##...#...#####......
               |......#.....####......#....#...
               |.........#..#..#...#....#.#....
               |....#........#...##....#.......
               |...##.#...#..........#....#....
               |........#.......#.##..#..#...#.
               |....#..##...........#.....#..#.
               |#....#...............#.#....#..
               |.#........#....#.#...#.......#.
               |#.......##..........#.......#..
               |...#....#...##.#..#.......#....
               |#..#.##...#.#...#...#...#....#.
               |###...#...#....#....#....#...#.
               |##......#.#.......#....#..#....
               |......#......#....#.#.#..###..#
               |.#.#.##.....#..#..........#....
               |##...#.#.#..##....#.....#.#....
               |#.##...#...#.#...####..#.......
               |.....##..#.#.#....#..##..#.#...
               |....###.#.........##.....#.....
               |......##...........#........#.#
               |.#.........##..................
               |.........##...#.............#.#
               |......##...#...#.........#..##.
               |#..#.......#..##.......###.....
               |....#.#.....#............##....
               |.....#..#......#....#.....##...
               |##......##...................#.
               |#....#............#.#.###.##...
               |.#.....#........#.....#...#....
               |......##.......######......##..
               |.#....##....#..###....#.......#
               |..............##.#..#.......#.#
               |.#..#..........#..#.##.........
               |......##.#..#......#.#....##.#.
               |#.....#.##...#.....#...#..#...#
               |.#....#..##.....#.....#.#.#....
               |..#......#.##..#.........#.#.#.
               |.#..##...#...#.....#..#..#.#..#
               |#.#.##.##.................#.#.#
               |.#..#.#..##.#.......#.......##.
               |#...#...#..##...#...##...#...#.
               |....#......#..#...#.....##..#..
               |..............##......#...#.#..
               |..##..#.......#..#..###.#.#....
               |.#..#..#...#.......#...#...##.#
               |.#...#.......###..#.##.###.....
               |##.#...#......#.....#..#.......
               |##....##............#.....#..#.
               |.....#...##......##.....#....##
               |#...##..#....#..##....###.#...#
               |.....#..#.#.....#.##..##....#..
               |.#.....#.#........#...#.#......
               |......#....#.#........#.#......
               |.##..#...............###...##.#
               |.......###.#.#......###.....#..
               |.......#..##...#....#.##..#.##.
               |..#.......##.......#.....#....#
               |.#......#....#..##..#.#.#..##..
               |###......#...#..#.............#
               |.#....#..#.#......##...........
               |.#....#.##.....#..#.......#..##
               |....#...#...#..#.....#..##..#.#
               |#.#.#.......##.#..#.#....#.....
               |##.#.......#...#...#.#......##.
               |#....#.#...........#######.....
               |...#.#.##.#......##..###.......
               |..#.#....#..#.................#
               |........#..##..#.....#....#.##.
               |...#.#..#..#..#..............##
               |.##.......###.#......#....#..##
               |..##.##.#......#....#..#...#..#
               |""".stripMargin
}

package aoc2020

object Day17 {

  def run() = { //part 2
    val data = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data17")
    println(data)
    var space = empty(25, 25, 25, 25)
    data.zipWithIndex.foreach {
      case (row, yind) =>
        row.zipWithIndex.foreach {
          case (c, xind) =>
            space = set(space, xind+10, yind+10, 12, 12, c)
        }
    }
    //printSpace(space)
    println(countTotal(space))
    (0 until 6).foreach { _ =>
      space = step(space)
      //printSpace(space)
      println(countTotal(space))
    }
    println(s"Part 2: ${countTotal(space)}")

  }

  def printSpace(space: Seq[Seq[Seq[String]]]) =
    space.zipWithIndex.foreach {
      case (plane, z) =>
        println(z)
        println(plane.mkString("\n"))
    }

  def empty(x: Int, y: Int, z: Int, w: Int): Seq[Seq[Seq[String]]] = {
    Seq.fill(w)(
      Seq.fill(z)(
        Seq.fill(y)(
          "."*x
        )
      )
    )
  }
  def set(space: Seq[Seq[Seq[String]]], x: Int, y: Int, z: Int, w: Int, c: Char): Seq[Seq[Seq[String]]] = {
    space.updated(w,
      space(w).updated(z,
        space(w)(z).updated(y,
          space(w)(z)(y).updated(x, c)
        )
      )
    )
  }

  def step(space: Seq[Seq[Seq[String]]]): Seq[Seq[Seq[String]]] = {
    var newSpace: Seq[Seq[Seq[String]]] = space
    val X = newSpace.head.head.head.length
    val Y = newSpace.head.head.length
    val Z = newSpace.head.length
    val W = newSpace.length
    (0 until X).foreach{
      x =>
        (0 until Y).foreach {
          y =>
            (0 until Z).foreach {
              z =>
              (0 until W).foreach {
                w =>
                  val neighs = countNeighbours(space, x, y, z, w)
                  if (space(w)(z)(y)(x) == '.' && neighs == 3) {
                    newSpace = set(newSpace, x, y, z, w, '#')
                  } else if (space(w)(z)(y)(x) == '#' && (neighs < 2 || neighs > 3)) {
                    newSpace = set(newSpace, x, y, z, w, '.')
                  }
              }
            }
        }
    }
    newSpace
  }
  def countNeighbours(space: Seq[Seq[Seq[String]]], x: Int, y: Int, z: Int, w: Int): Int = {
    val delta = Seq(-1, 0, 1)
    val X = space.head.head.head.length
    val Y = space.head.head.length
    val Z = space.head.length
    val W = space.length
    delta.map {
      dx =>
        delta.map {
          dy =>
          delta.map {
            dz =>
            delta.map {
              dw =>
                if (dx == 0 && dy == 0 && dz == 0 && dw == 0) {
                  0
                } else if (dx+x >= 0 && dy+y >= 0 && dz+z >= 0 && dw+w >= 0 && dx+x < X && dy+y < Y && dz+z < Z && dw+w < W && space(dw+w)(dz+z)(dy+y)(dx+x) == '#') {
                  1
                } else 0
            }.sum
          }.sum
        }.sum
    }.sum
  }
  def countTotal(space: Seq[Seq[Seq[String]]]) = {
    space.map(
      W => W.map(
        plane => plane.map(
          row => row.count(_ == '#')
        ).sum
      ).sum
    ).sum
  }
}

object Day17space3 {

  def run() = { //part 1
    val data = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data17")
    println(data)
    var space = empty(25, 25, 25)
    data.zipWithIndex.foreach {
      case (row, yind) =>
        row.zipWithIndex.foreach {
          case (c, xind) =>
            space = set(space, xind+10, yind+10, 12, c)
        }
    }
    //printSpace(space)
    println(countTotal(space))
    (0 until 6).foreach { _ =>
      space = step(space)
      //printSpace(space)
      println(countTotal(space))
    }
    println(s"Part 1: ${countTotal(space)}")

  }

  def printSpace(space: Seq[Seq[String]]) =
    space.zipWithIndex.foreach {
      case (plane, z) =>
        println(z)
        println(plane.mkString("\n"))
    }

  def empty(x: Int, y: Int, z: Int): Seq[Seq[String]] = {
    Seq.fill(z)(
      Seq.fill(y)(
        "."*x
      )
    )
  }
  def set(space: Seq[Seq[String]], x: Int, y: Int, z: Int, c: Char) = {
    space.updated(z,
      space(z).updated(y,
        space(z)(y).updated(x, c)
      )
    )
  }

  def step(space: Seq[Seq[String]]): Seq[Seq[String]] = {
    var newSpace: Seq[Seq[String]] = space
    val X = newSpace.head.head.length
    val Y = newSpace.head.length
    val Z = newSpace.length
    (0 until X).foreach{
      x =>
        (0 until Y).foreach {
          y =>
            (0 until Z).foreach {
              z =>
                val neighs = countNeighbours(space, x, y, z)
                if (space(z)(y)(x) == '.' && neighs == 3) {
                  newSpace = set(newSpace, x, y, z, '#')
                } else if (space(z)(y)(x) == '#' && (neighs < 2 || neighs > 3)) {
                  newSpace = set(newSpace, x, y, z, '.')
                }
            }
        }
    }
    newSpace
  }
  def countNeighbours(space: Seq[Seq[String]], x: Int, y: Int, z: Int): Int = {
    val delta = Seq(-1, 0, 1)
    val X = space.head.head.length
    val Y = space.head.length
    val Z = space.length
    delta.map {
      dx =>
        delta.map {
          dy =>
            delta.map {
              dz =>
                if (dx == 0 && dy == 0 && dz == 0) {
                  0
                } else if (dx+x >= 0 && dy+y >= 0 && dz+z >= 0 && dx+x < X && dy+y < Y && dz+z < Z && space(dz+z)(dy+y)(dx+x) == '#') {
                  1
                } else 0
            }.sum
        }.sum
    }.sum
  }
  def countTotal(space: Seq[Seq[String]]) = {
    space.map(plane => plane.map(row => row.count(_ == '#')).sum).sum
  }
}

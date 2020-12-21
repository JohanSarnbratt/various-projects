package aoc2020

object Day20 {
  def run(): Unit = {
    val testStartPieces = part1("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data20test")
    val startPiece = part1("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data20")
    part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data20test", testStartPieces.head)
    //part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data20test", testStartPieces(1))
    //part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data20test", testStartPieces(2))
    //part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data20test", testStartPieces(3))
    part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data20", startPiece.head)
    part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data20", startPiece(1))
    part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data20", startPiece(2))
    part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data20", startPiece(3))
    part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data20", startPiece.head, true)
    part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data20", startPiece(1), true)
    part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data20", startPiece(2), true)
    part2("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data20", startPiece(3), true)
  }

  def part1(fileName: String): Seq[Int] = {
    val data: Seq[Seq[String]] = AocHelpers.readDataGroupsSeparatedByBlankLines(fileName)
    val edgeNumbers = getMinEdgeNumbers(data)
    val pieceEdgeNumbers: Seq[(Int, Int)] = getPieceEdgeNumbers(data)
    val pieceEdginess = pieceEdgeNumbers.filter {
      case (edge, _) => edgeNumbers.count(_ == edge) < 2
    }.map {
      case (_, piece) => piece
    }
    val cornerPieces = pieceEdginess.groupBy(x => x).filter {
      case (_, pieces) => pieces.length > 1
    }
    println(s"part1: ${cornerPieces.keys}")
    println(s"part1: ${cornerPieces.keys.map(_.toLong).product}")
    cornerPieces.keys.toSeq
  }
  def part2(fileName: String, startPieceName: Int, mirror: Boolean = false): Unit = {
    val data: Seq[Seq[String]] = AocHelpers.readDataGroupsSeparatedByBlankLines(fileName)
    val edgeNumbers: Seq[Int] = getMinEdgeNumbers(data)
    val pieces: Seq[Piece] = data.map {
      strings =>
        val pieceNumber = Integer.parseInt(strings.head.slice(5,9))
        Piece(pieceNumber, strings.tail)
    }
    val edgeMap: Seq[(String, (Int, Int))] = pieces.flatMap { piece =>
      val edges = getEdgesFromPiece(piece.image).zipWithIndex.map {
        case (edge, edgeInd) => edge -> (piece.name, edgeInd)
      }
      val segde = getEdgesFromPiece(piece.mirror().image).zipWithIndex.map {
        case (edge, edgeInd) => edge -> (piece.name, edgeInd+4)
      }
      edges++segde
    }
    //val piecesAlongOneEdge: Int = (Math.sqrt(pieces.length)+0.5).toInt
    //val board: Seq[Seq[Option[Piece]]] = Seq.fill(piecesAlongOneEdge)(Seq.fill(piecesAlongOneEdge)(None))
    //find orientation of startPiece
    val startPiece = if (mirror) pieces.find(_.name == startPieceName).get.mirror()
    else pieces.find(_.name == startPieceName).get
    val startPieceEdgeNum = getEdgesFromPiece(startPiece.image).map(getMinNumFromEdge)
    val distinctness = startPieceEdgeNum.map(en => edgeNumbers.count(en == _))
    //println(distinctness)
    val rotatedStartPiece = distinctness match {
      case Seq(1,1,2,2) => startPiece
      case Seq(2,1,1,2) => startPiece.rotate(1)
      case Seq(2,2,1,1) => startPiece.rotate(2)
      case Seq(1,2,2,1) => startPiece.rotate(3)
    }
    val board = buildBoard(rotatedStartPiece, pieces, edgeMap, 0)
    //printBoard(board)
    //println(board.map(_.map(_.name)).mkString("\n"))
    val boardNoEdges = board.map(_.map(_.removeEdges()))
    //printBoard(boardNoEdges)
    val image = mergeBoard(boardNoEdges).map(_.mkString(""))
    //println(image.mkString("\n"))
    //println(image.mkString("\n").count(_ == '#'))
    val monsters = findSeaMonster(image)
    println(image.mkString("\n").count(_ == '#')-monsters*15)
  }
  val seaMonster = "                  # #    ##    ##    ### #  #  #  #  #  #   "
  val seaMonsterLength = seaMonster.length/3
  val seaMonsterSize = seaMonster.count(_ == '#')
  val sminds = seaMonster.zipWithIndex.filter(_._1 == '#').map(_._2)
  val smcoords = sminds.map(smi => (smi%seaMonsterLength,smi/seaMonsterLength))
  def findSeaMonster(image: Seq[String]) = {
    (0 to image.head.length-seaMonsterLength).map{x =>
      (0 to image.head.length-3).count{y =>
        smcoords.forall{case (sx,sy) => image(y+sy)(x+sx) == '#'}
      }
    }.sum
  }
  def mergeBoard(board: Seq[Seq[Piece]]) = {
    board.flatMap{
      row => row.head.image.indices.map(i => row.flatMap(_.image(i)))
    }
  }
  def printBoard(board: Seq[Seq[Piece]]) = {
    board.foreach{row =>
      printRow(row)
      println("")
    }
  }
  def printRow(pieces: Seq[Piece]) = {
    pieces.head.image.indices.foreach{ ind =>
      println(pieces.map(_.image(ind)).mkString(" "))
    }
  }
  def buildBoard(startPiece: Piece, pieces: Seq[Piece], edgeMap: Seq[(String, (Int, Int))], depth: Int): Seq[Seq[Piece]] = {
    val firstColumn = buildRow(startPiece.mirror().rotate(1), pieces, edgeMap, 0)
    firstColumn.map{
      piece => buildRow(piece.mirror().rotate(1), pieces, edgeMap, 0)
    }
  }

  /**
   * Starting with startPiece tries to find a matching piece to the right until there is no more pieces
   */
  def buildRow(startPiece: Piece, pieces: Seq[Piece], edgeMap: Seq[(String, (Int, Int))], depth: Int): Seq[Piece] = {
    //println(startPiece.name)
    //startPiece.pretty()
      Seq(startPiece) ++ findToRight(startPiece: Piece, pieces, edgeMap).map {
        piece => buildRow(piece, pieces, edgeMap, depth+1)
      }.getOrElse(Seq.empty)

  }

  /**
   * Finds one piece that matches being to the right of startPiece
   */
  def findToRight(startPiece: Piece, pieces: Seq[Piece], edgeMap: Seq[(String, (Int, Int))]): Option[Piece] = {
    val rightEdge: String = getEdgesFromPiece(startPiece.image)(2).reverse
    findMatchingEdge(rightEdge, startPiece.name, edgeMap).map {
      case (pieceName, orientation) =>
        val piece = pieces.find(_.name == pieceName).get
        orientation match {
          case 0 => piece
          case 1 => piece.rotate(1)
          case 2 => piece.rotate(2)
          case 3 => piece.rotate(3)
          case 4 => piece.mirror()
          case 5 => piece.mirror().rotate(1)
          case 6 => piece.mirror().rotate(2)
          case 7 => piece.mirror().rotate(3)
        }
    }
  }

  /**
   * Finds an edge
   * @param edge
   * @param notPieceName
   * @param edgeMap
   * @return
   */
  def findMatchingEdge(edge: String, notPieceName: Int, edgeMap: Seq[(String, (Int, Int))]): Option[(Int, Int)] = {
    val res = edgeMap.filter {
      case (innerEdge, (pieceName, _)) => edge == innerEdge && pieceName != notPieceName
    }
    if (res.length > 1)
      println(s"Somethings wrong: $edge $notPieceName $res")
    res.headOption.map(_._2)
  }
  case class Piece(name: Int, image: Seq[String]) {
    def rotate(quarters: Int): Piece = {
      val size = image.length
      quarters match {
        case 0 => this
        case 1 =>
          val newImage = (0 until size).map {
            ind => image.map(str => str(size-ind-1)).mkString
          }
          Piece(name, newImage)
        case 2 =>
          Piece(name, image.reverse.map(_.reverse))
        case 3 =>
          val newImage = (0 until size).map {
            ind => image.map(str => str(ind)).mkString.reverse
          }
          Piece(name, newImage)
      }
    }
    def mirror(): Piece = Piece(name, image.map(_.reverse))
    def pretty() = {
      image.foreach(println(_))
    }
    def removeEdges() = {
      Piece(name, image.tail.dropRight(1).map(_.tail.dropRight(1)))
    }
  }
  def getMinEdgeNumbers(data: Seq[Seq[String]]): Seq[Int] = {
    data.flatMap {
      piece =>
        getEdgesFromPiece(piece.tail).map(getMinNumFromEdge)
    }
  }
  def getPieceEdgeNumbers(data: Seq[Seq[String]]): Seq[(Int, Int)] = {
    data.flatMap {
      piece =>
        val pieceNumber = Integer.parseInt(piece.head.slice(5,9))
        getEdgesFromPiece(piece.tail).map(getMinNumFromEdge).map(_ -> pieceNumber)
    }
  }
  /*
  ^ 1--->
  |     2
  |     |
  0     |
  <---3 v
   */
  def getEdgesFromPiece(piece: Seq[String]): Seq[String] = {
    Seq(
      piece.flatMap((row: String) => row.head.toString).mkString.reverse,
      piece.head,
      piece.flatMap((row: String) => row.last.toString).mkString,
      piece.last.reverse,
    )
  }
  def getMinNumFromEdge(edge: String): Int = {
    val bin = edge.replace('.','0').replace('#','1')
    val num = Integer.parseInt(bin, 2)
    val numFlipped = Integer.parseInt(bin.reverse, 2)
    Math.min(num, numFlipped)
  }
}

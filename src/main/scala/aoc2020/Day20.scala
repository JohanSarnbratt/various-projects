package aoc2020

object Day20 {

  def run() = { //part 2
    val data: Seq[Seq[String]] = AocHelpers.readDataGroupsSeparatedByBlankLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data20")
    val edgeNumbers = getEdgeNumbers(data)
    println(data.length)
    println(edgeNumbers.length)
    println(data.length*4-12*11*2)
    val distinctEdges = edgeNumbers.distinct
    println(distinctEdges.length)
    val pieceEdgeNumbers: Seq[(Int, Int)] = getPieceEdgeNumbers(data)
    val pieceEdginess = pieceEdgeNumbers.filter {
      case (edge, _) => edgeNumbers.count(_ == edge) < 2
    }.map {
      case (_, piece) => piece
    }
    println(pieceEdginess)
    val cornerPieces = pieceEdginess.groupBy(x => x).filter {
      case (_, pieces) => pieces.length > 1
    }
    println(s"part1: ${cornerPieces.keys.map(_.toLong).product}")
  }
  def getEdgeNumbers(data: Seq[Seq[String]]) = {
    data.flatMap{
      piece =>
        getEdgesFromPiece(piece.tail).map(getMinNumFromEdge)
    }
  }
  def getPieceEdgeNumbers(data: Seq[Seq[String]]) = {
    data.flatMap {
      piece =>
        val pieceNumber = Integer.parseInt(piece.head.slice(5,9))
        getEdgesFromPiece(piece.tail).map(getMinNumFromEdge).map(_ -> pieceNumber)
    }
  }
  def getEdgesFromPiece(piece: Seq[String]) = {
    Seq(piece.head,
    piece.last,
    piece.flatMap((row: String) => row.head.toString).mkString,
    piece.flatMap((row: String) => row.last.toString).mkString)
  }
  def getMinNumFromEdge(edge: String) = {
    val bin = edge.replace('.','0').replace('#','1')
    val num = Integer.parseInt(bin, 2)
    val numFlipped = Integer.parseInt(bin.reverse, 2)
    Math.min(num, numFlipped)
  }
}

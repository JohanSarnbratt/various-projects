
import scala.collection.mutable
import scala.collection.immutable

object Boardie {
  def newBoard(size: Int): Board = {
    val board: immutable.Seq[immutable.Seq[Boolean]] = immutable.Seq.range(1, size+1)
      .map(
        e =>
          immutable.Seq.range(0,e)
            .map((_: Int) => true)
      )
    new Board(size, board, immutable.Seq.empty)
  }
}

class Board(size: Int, val board: immutable.Seq[immutable.Seq[Boolean]], val moves: immutable.Seq[Move]) {

  def validateMove(yxyx: Move, printIt: Boolean = false): Boolean = {
    validateMove(yxyx.from.y, yxyx.from.x, yxyx.to.y, yxyx.to.x, printIt)
  }
  def validateMove(y1: Int, x1: Int, y2: Int, x2: Int, printIt: Boolean): Boolean = {
    val dy = Math.abs(y2-y1)
    val my = (y2+y1)/2
    val dx = Math.abs(x2-x1)
    val mx = (x2+x1)/2
    val valid = (dx == 0 || dx == 2) && (dy == 0 || dy == 2) &&
      x1<=y1 && x2<=y2 &&
      0<=y1 && 0<=x1 && 0<=y2 && 0<=x2 &&
      y1<size && y2<size &&
      board(y1)(x1) && board(my)(mx) && !board(y2)(x2)
    if (printIt) println(s"($y1, $x1, $y2, $x2) is allowed: $valid")
    valid
  }
  def move(yxyx: Move): Board = {
    move(yxyx.from.y, yxyx.from.x, yxyx.to.y, yxyx.to.x)
  }
  def move(y1: Int, x1: Int, y2: Int, x2: Int): Board = {
    val my = (y2+y1)/2
    val mx = (x2+x1)/2
    val by1 = board.updated(y1, board(y1).updated(x1, false))
    val bmy = by1.updated(my, by1(my).updated(mx, false))
    val by2 = bmy.updated(y2, bmy(y2).updated(x2, true))
    new Board(size, by2, moves :+ Move(Pos(y1, x1), Pos(y2, x2)))
  }
  def printBoard(): Unit = {
    board.foreach(
      (row: immutable.Seq[Boolean]) => {
        print(" "*(size-row.length))
        row.foreach(value => if (value) print("X ") else print("' "))
        println("")
      }
    )
  }
  def initialMove(p: Pos): Board = {
    println(s"Making initial move $p on size $size")
    new Board(size, board.updated(p.y, board(p.y).updated(p.x, false)), moves :+ Move(Pos(p.y, p.x), Pos(0, 0)))
  }
  /*def copy(): Board = {
    val newBoard = new Board(size)
    newBoard.board = board.clone()
    newBoard.moves = moves.clone()
    newBoard
  }*/
  val allPositions: immutable.Seq[Pos] = {
    immutable.Seq.range(0, size)
      .flatMap(
        y =>
          immutable.Seq.range(0, y+1)
            .map(x => Pos(y, x))
      )
  }
  val directions = immutable.Seq((2,2),(2,0),(0,2),(-2,-2),(-2,0),(0,-2))
  lazy val possibleMoves: immutable.Seq[Move] = {
    val o: immutable.Seq[Move] = allPositions.flatMap(yx =>
      directions.map(dyx =>
        Move(yx, Pos(yx.y+dyx._1, yx.x+dyx._2))
      )
    )
    o.filter(xxx => validateMove(xxx))
  }
  def isSolved(): Boolean = {
    board.flatten.count(x => x) == 1
  }

  /**
   *  0
   *  1, 2
   *  3, 4, 5
   *  6, 7, 8, 9
   * 10,11,12,13,14
   *
   * 1,2,4,7,11
   * 0,1,3,6,10,16
   *
   * 0,1,4,
   *
   * (0,0)
   * (1,0) (1,1)
   * (2,0) (2,1) (2,2)
   * (3,0) (3,1) (3,2) (3,3)
   * (4,0) (4,1) (4,2) (4,3) (4,4)
   * (5,0) (5,1) (5,2) (5,3) (5,4) (5,5)
   */
}
case class Pos(y: Int, x: Int)
case class Move(from: Pos, to: Pos)
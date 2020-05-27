package trianglePeg

import scala.collection.immutable.Seq

object Boardie {
  def newBoard(size: Int): Board = {
    val board: Seq[Seq[Boolean]] = Seq.range(1, size+1)
      .map(
        e =>
          Seq.range(0,e)
            .map((_: Int) => true)
      )
    new Board(size, board, Seq.empty)
  }
}

class Board(size: Int, val board: Seq[Seq[Boolean]], val moves: Seq[Move], oldPossibleMoves: Option[Seq[Move]] = None) {
  lazy val hash = board.hashCode()

  def validateMove(move: Move, printIt: Boolean = false): Boolean = {
    val dy = Math.abs(move.to.y-move.from.y)
    val my = (move.to.y+move.from.y)/2
    val dx = Math.abs(move.to.x-move.from.x)
    val mx = (move.to.x+move.from.x)/2
    val valid = (dx == 0 || dx == 2) && (dy == 0 || dy == 2) &&
      move.from.x<=move.from.y && move.to.x<=move.to.y &&
      0<=move.from.y && 0<=move.from.x && 0<=move.to.y && 0<=move.to.x &&
      move.from.y<size && move.to.y<size &&
      board(move.from.y)(move.from.x) && board(my)(mx) && !board(move.to.y)(move.to.x)
    if (printIt) println(s"$move is allowed: $valid")
    valid
  }
  def move(move: Move): Board = {
    val my = (move.from.y+move.to.y)/2
    val mx = (move.from.x+move.to.x)/2
    val by1 = board.updated(move.from.y, board(move.from.y).updated(move.from.x, false))
    val bmy = by1.updated(my, by1(my).updated(mx, false))
    val by2 = bmy.updated(move.to.y, bmy(move.to.y).updated(move.to.x, true))
    new Board(size, by2, moves :+ move)
  }
  def printBoard(): Unit = {
    board.foreach(
      (row: Seq[Boolean]) => {
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
  lazy val allPositions: Seq[Pos] = {
    Seq.range(0, size)
      .flatMap(
        y =>
          Seq.range(0, y+1)
            .map(x => Pos(y, x))
      )
  }
  private val directions = Seq(Pos(2,2),Pos(2,0),Pos(0,2),Pos(-2,-2),Pos(-2,0),Pos(0,-2))
  lazy val possibleMoves: Seq[Move] = {
    oldPossibleMoves match {
      case None =>
        allPositions
          .flatMap(yx =>
            directions.map(d =>
              Move(yx, Pos(yx.y+d.y, yx.x+d.x))
            )
          ).filter(xxx => validateMove(xxx))
      case Some(possibleMoves) =>
        val lastMove = moves.last
        val lastJumped = Pos((lastMove.from.y+lastMove.to.y)/2, (lastMove.from.x+lastMove.to.x)/2)
        val keepFromOld = possibleMoves.filter((move: Move) => {
          val jump = Pos((move.from.y + move.to.y) / 2, (move.from.x + move.to.x) / 2)
          if (move.from == lastMove.from) false
          else if (move.from == lastJumped) false
          else if (jump == lastMove.from) false
          else if (jump == lastJumped) false
          else if (move.to == lastMove.to) false
          else true
        })
        val newMoves = directions.map(d =>
          Move(lastMove.to, Pos(lastMove.to.y+d.y, lastMove.to.x+d.x))
        ).filter(xxx => validateMove(xxx))
        keepFromOld ++ newMoves
    }
  }//.sortBy(move => move.from.x+move.from.y)
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
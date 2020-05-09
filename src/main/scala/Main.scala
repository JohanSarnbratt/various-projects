
object Main extends App {
  def demo = {
    val board1 = Boardie.newBoard(4)
    board1.printBoard()
    val board2 = board1.initialMove(Pos(1, 0))
    board2.printBoard()
    board2.validateMove(3, 2, 1, 0, printIt = true)
    val board6 = board2.move(3, 2, 1, 0)
    board6.printBoard()
    println(board6.allPositions)
  }
  def run() = {
    val startTime = System.nanoTime()
    val board = Boardie.newBoard(6).initialMove(Pos(0, 0))
    board.printBoard()
    solve(board, 0, 65732489, Pos(1,0), startTime)
    val endTime = System.nanoTime()
    println(s"Finished in ${(endTime-startTime)/1000.0/1000.0/1000.0}s")
  }
  def solve(board: Board, countMoves: Int, record: Int, lastMove: Pos, startTime: Long): Int = {
    if (board.isSolved()) {
      val endTime = System.nanoTime()
      println(s"Found new record of $countMoves in ${(endTime-startTime)/1000.0/1000.0/1000.0}s")
      println(board.moves)
      return countMoves
    }
    if (countMoves >= record) {
      return 32748
    }
    val moves = board.possibleMoves
    var newRecord = record
    moves.foreach((move: Move) => {
      //board.validateMove(move, printIt = true)
      val newBoard = board.move(move)
      //newBoard.printBoard()
      val newMove = if (lastMove.y != move.from.y || lastMove.x != move.from.x) 1 else 0
      val newVal = if (countMoves+newMove < newRecord) solve(newBoard, countMoves+newMove, newRecord, move.to, startTime) else 34554
      if (newVal<newRecord) {
        newRecord = newVal
      }
      newVal
    })
    newRecord
  }
  //demo
  run()
}
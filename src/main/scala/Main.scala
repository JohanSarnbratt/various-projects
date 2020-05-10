import scala.collection.mutable

object Main extends App {
  val size = 6
  val bigNumber = size*size+7
  def demo = {
    val move = Move(Pos(3, 2), Pos(1, 0))
    val board1 = Boardie.newBoard(4)
    board1.printBoard()
    val board2 = board1.initialMove(Pos(1, 0))
    board2.printBoard()
    board2.validateMove(move, printIt = true)
    val board6 = board2.move(move)
    board6.printBoard()
    println(board6.allPositions)
  }
  def run() = {
    val initialMove = Pos(1, 0)
    val startTime = System.nanoTime()
    val board = Boardie.newBoard(size).initialMove(initialMove)
    board.printBoard()
    solve(board, 0, bigNumber, initialMove, startTime)
    val endTime = System.nanoTime()
    println(s"Finished in ${(endTime-startTime)/1000.0/1000.0/1000.0}s")
  }
  val hashmap = mutable.HashMap.empty[Int, Seq[Move]]
  val useHash = false
  def solve(board: Board, countMoves: Int, record: Int, lastMove: Pos, startTime: Long): Int = {
    if (board.isSolved()) {
      if (useHash)
        hashmap.addOne((board.hash, Seq.empty))
      val endTime = System.nanoTime()
      println(s"Found new record of $countMoves in ${(endTime-startTime)/1000.0/1000.0/1000.0}s")
      println(board.moves)
      return countMoves
    }
    if (countMoves >= record) {
      return bigNumber
    }
    val moves = board.possibleMoves
    var newRecord = record
    moves.foreach((move: Move) => {
      //board.validateMove(move, printIt = true)
      val newBoard = board.move(move)
      //newBoard.printBoard()
      val newMove = if (lastMove.y != move.from.y || lastMove.x != move.from.x) 1 else 0
      val newVal = if (countMoves+newMove < newRecord) {
        if (useHash && hashmap.contains(newBoard.hash)) {
          //println("Took a shortcut")
          val shortcutVal = countMoves + newMove + hashmap(newBoard.hash).length
          if (shortcutVal<newRecord) {
            val endTime = System.nanoTime()
            println(s"Found new record of $shortcutVal in ${(endTime-startTime)/1000.0/1000.0/1000.0}s with a shortcut (shortcutlength: ${hashmap(newBoard.hash).length})")
            println(board.moves++Seq(move)++hashmap(newBoard.hash))
          }
          shortcutVal
        } else
          solve(newBoard, countMoves+newMove, newRecord, move.to, startTime)
      } else bigNumber
      if (newVal<newRecord) {
        newRecord = newVal
        if (useHash)
          hashmap.addOne((board.hash, Seq(move)++hashmap(newBoard.hash)))
      }
      newVal
    })
    newRecord
  }
  //demo
  run()
}
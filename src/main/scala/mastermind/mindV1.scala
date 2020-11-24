package mastermind

object mindV1 {
  def figureItOut(balls: Int, colors: Int, duplicates: Int) = {
    val board = new Board(balls, colors, duplicates, showSolutionAtStart = false)
    val initialGuess = 0 until balls
    val initialScore = board.makeGuess(initialGuess)
    var bestGuess = initialScore
    pr(s"$initialGuess $initialScore")
    while (bestGuess._1 < balls) {
      val newGuess = findConsistent(balls, colors, duplicates, board.getGuesses())
      val newGuessScore = board.makeGuess(newGuess)
      if (newGuessScore._1 > bestGuess._1)
        bestGuess = newGuessScore
      pr(s"$newGuess $newGuessScore")
    }
    pr(s"Correct after ${board.getGuesses().length} guesses")
    board.getGuesses().length
  }

  def findConsistent(balls: Int, colors: Int, duplicates: Int, oldGuesses: Seq[(Seq[Int], (Int, Int))]) = {
    var newGuess = Board.generateRandomValid(balls, colors, duplicates)
    while (!oldGuesses.forall(oldGuess => isConsistent(newGuess, oldGuess))) {
      newGuess = Board.generateRandomValid(balls, colors, duplicates)
    }
    newGuess
  }

  def isConsistent(newGuess: Seq[Int], oldGuessStats: (Seq[Int], (Int, Int))): Boolean = {
    val correctPosition = oldGuessStats._2._1
    val correctColour = oldGuessStats._2._2
    val oldGuess = oldGuessStats._1
    val consistentPositions = newGuess.zip(oldGuess).map(gs => gs._2 == gs._1)
    val numberOfConsistentPositions = consistentPositions.count(identity)
    val (newColors, oldColors) = newGuess.zip(oldGuess).filterNot(gs => gs._2 == gs._1).unzip
    val consistentColors = newColors.intersect(oldColors).length
    //pr(s"Comparing $newGuess with $oldGuessStats. consistentPositions: $consistentPositions")
    newGuess != oldGuess && correctPosition == numberOfConsistentPositions && correctColour == consistentColors
  }

  private def pr(p: Any, show: Boolean = false): Unit = {
    if (show) println(p)
  }
}

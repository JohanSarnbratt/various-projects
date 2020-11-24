package mastermind

import scala.collection.mutable
import scala.util.Random

object mAIstermind {
  def run() = {
    //val board = new Board(6, 8, 3)
    //pr(board.makeGuess(Seq(1,2,1,2,1,2)))
    mindV2.consistency(Seq(Some(4), Some(0)), List(0,1))
    val balls = 4
    val colors = 6
    val duplicates = 4
    pr(s"balls: $balls, colors: $colors, duplicates: $duplicates", true)
    val tries = 1000
    pr(s"tries: $tries", true)
    val noGuesses = (0 until tries).map(_ => mindV2.figureItOut(balls, colors, duplicates))
    val avgNoGuesses = noGuesses.sum * 1.0 / tries
    pr(s"Average no of guesses: $avgNoGuesses, max: ${noGuesses.max}", true)
    (1 to noGuesses.max).foreach(guessNo => pr(s"did $guessNo guesses ${noGuesses.count(i => i == guessNo)}", true))
  }

  private def pr(p: Any, show: Boolean = false): Unit = {
    if (show) println(p)
  }
}

trait mind {
  type Color = Int

  def figureItOut(balls: Int, colors: Int, duplicates: Int, solution: Option[Seq[Int]]) = {
    val board = new Board(balls, colors, duplicates, showSolutionAtStart = false)
    if (solution.isDefined) board.setSolution(solution.get)
    val initialGuess = (0 until balls).map(_/2)
    val initialScore = board.makeGuess(initialGuess)
    var bestGuess = initialScore
    pr(s"$initialGuess $initialScore")
    while (bestGuess._1 < balls) {
      pr("")
      pr("")
      pr("")
      pr(s"Old guesses: ${board.getGuesses()}")
      val newGuess = findConsistent(balls, colors, duplicates, board.getGuesses())
      val newGuessScore = board.makeGuess(newGuess)
      if (newGuessScore._1 > bestGuess._1)
        bestGuess = newGuessScore
      pr(s"$newGuess $newGuessScore")
      //scala.io.StdIn.readLine()
    }
    pr(s"Correct after ${board.getGuesses().length} guesses")
    board.getGuesses().length
  }

  def findConsistent(balls: Int, colors: Int, duplicates: Int, oldGuesses: Seq[(Seq[Color], (Int, Int))]): Seq[Color]

  def pr(p: Any, show: Boolean = false): Unit = {
    if (show) println(p)
  }
}
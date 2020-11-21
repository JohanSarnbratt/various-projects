package mastermind

import scala.collection.mutable
import scala.util.Random

class Board(balls: Int, colors: Int, duplicates: Int = 1, showSolutionAtStart: Boolean = true) {
  private val solution: Seq[Int] = Board.generateRandomValid(balls, colors, duplicates)
  pr(solution, showSolutionAtStart)
  private var guesses: mutable.Seq[(Seq[Int], (Int, Int))] = mutable.Seq.empty

  def makeGuess(guess: Seq[Int]): (Int, Int) = {
    require(guess.length == solution.length)
    val correctPositions = guess.zip(solution).map(gs => gs._2 == gs._1)
    val numberOfCorrectPositions = correctPositions.count(identity)
    val otherGuessColors: Seq[Int] = guess.zip(correctPositions).filterNot(gp => gp._2).map(_._1)
    val otherSolutionColors = solution.zip(correctPositions).filterNot(gp => gp._2).map(_._1)
    val numberOfCorrectColors = otherGuessColors.intersect(otherSolutionColors).length
    guesses = guesses.appended((guess, (numberOfCorrectPositions, numberOfCorrectColors)))
    (numberOfCorrectPositions, numberOfCorrectColors)
  }

  def getGuesses(): Seq[(Seq[Int], (Int, Int))] = guesses.toSeq

  private def pr(p: Any, show: Boolean = true): Unit = {
    if (show) println(p)
  }
}

object Board {
  def generateRandomValid(balls: Int, colors: Int, duplicates: Int) = {
    Seq.fill(duplicates)(0 until colors).flatten.sortBy(_ => Random.between(0.0,1.0)).take(balls)
  }
}
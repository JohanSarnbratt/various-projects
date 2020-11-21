package mastermind

import scala.collection.mutable

object mAIstermind {
  def run() = {
    //val board = new Board(6, 8, 3)
    //pr(board.makeGuess(Seq(1,2,1,2,1,2)))
    val balls = 4
    val colors = 6
    val duplicates = 4
    pr(s"balls: $balls, colors: $colors, duplicates: $duplicates", true)
    val tries = 1
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

object mindV2 {

  def figureItOut(balls: Int, colors: Int, duplicates: Int) = {
    val board = new Board(balls, colors, duplicates, showSolutionAtStart = false)
    val initialGuess = (0 until balls).toList
    val initialScore = board.makeGuess(initialGuess)
    var bestGuess = initialScore
    pr(s"$initialGuess $initialScore")
    while (bestGuess._1 < balls) {
      val newGuesses = findConsistent(balls, colors, duplicates, board.getGuesses())
      pr(newGuesses.toSet)
      val newGuess = newGuesses.head
      val newGuessScore = board.makeGuess(newGuess)
      if (newGuessScore._1 > bestGuess._1)
        bestGuess = newGuessScore
      pr(s"$newGuess $newGuessScore")
      //scala.io.StdIn.readLine()
    }
    pr(s"Correct after ${board.getGuesses().length} guesses")
    board.getGuesses().length
  }

  def findConsistent(balls: Int, colors: Int, duplicates: Int, oldGuesses: Seq[(Seq[Int], (Int, Int))]): Seq[Seq[Int]] = {
    val guessPosition = Seq.fill[Option[Int]](balls)(None)
    val guessColors = Seq()
    findConsistent(balls, colors, duplicates, oldGuesses, guessPosition, guessColors)
  }

  def findConsistent(balls: Int, colors: Int, duplicates: Int, oldGuesses: Seq[(Seq[Int], (Int, Int))], guessPosition: Seq[Option[Int]], guessColors: Seq[Int]): Seq[Seq[Int]] = {
    pr(guessPosition.map {
      case None => "*"
      case Some(x) => x.toString
    })
    if (oldGuesses.isEmpty) {
      if (guessPosition.contains(None)) {

        val possibleColorsToAdd = if (guessColors.nonEmpty) Seq(guessColors.head) else 0 until colors
        val newGuessColors = if (guessColors.nonEmpty) guessColors.tail else Seq()
        guessPosition.zipWithIndex.flatMap {
          case (None, ind) =>
            possibleColorsToAdd.flatMap { newColor =>
              val newGuessPosition = replace(guessPosition, ind, Some(newColor))
              findConsistent(balls, colors, duplicates, oldGuesses, newGuessPosition, newGuessColors)
            }
          case (_, _) => Seq.empty[Seq[Int]]
        }
      } else {
        Seq(guessPosition.flatten)
      }
    } else {
      val currentOldGuess: Seq[Int] = oldGuesses.head._1
      val currentOldGuessScore: (Int, Int) = oldGuesses.head._2
      val newGuessConsistency = consistency(guessPosition, currentOldGuess)
      //Todo add filter fun
      if (newGuessConsistency == currentOldGuessScore) {
        findConsistent(balls, colors, duplicates, oldGuesses.tail, guessPosition, guessColors)
      }
      else if (newGuessConsistency._1 > currentOldGuessScore._1 || newGuessConsistency._1 > currentOldGuessScore._1) {
        Seq.empty[Seq[Int]]
      }
      else if (newGuessConsistency._1 < currentOldGuessScore._1) {
        //add one more position guess
        guessPosition.zipWithIndex.flatMap {
          case (None, ind) =>
            val newGuessPosition = replace(guessPosition, ind, Some(currentOldGuess(ind)))
            findConsistent(balls, colors, duplicates, oldGuesses, newGuessPosition, guessColors)
          case (_, _) => Seq.empty[Seq[Int]]
        }
      } else if (newGuessConsistency._2 < currentOldGuessScore._2) {
        val numberOfColorsToAdd = currentOldGuessScore._2 - newGuessConsistency._2
        //add one more color guess
        //Todo: do not add colors that already exist in guessColors
        val possibleColorsToAdd = currentOldGuess.zip(guessPosition).filter {
          case (_, None) => true
          case _ => false
        }.map(_._1).zipWithIndex //zipWithIndex to make each element unique
        //take any numberOfColorsToAdd elements from possibleColorsToAdd
        possibleColorsToAdd
          .toSet
          .subsets(numberOfColorsToAdd)
          .map(_.toSeq.map(_._1))
          .toSeq
          .distinct
          .flatMap(colorsToAdd =>
            findConsistent(balls, colors, duplicates, oldGuesses.tail, guessPosition, guessColors++colorsToAdd)
          )

      } else {
        pr("This should never happen", show = true)
        Seq.empty[Seq[Int]]
      }
    }
  }

  def replace[T](seq: Seq[T], index: Int, value: T): Seq[T] = Seq(seq.take(index), Seq(value), seq.drop(index + 1)).flatten

  def isConsistent(a: (Int, Int), b: (Int, Int)) = a._1 == b._1 && a._2 == b._2

  def consistency(newGuess: Seq[Option[Int]], oldGuess: Seq[Int]): (Int, Int) = {
    val consistentPositions = newGuess.zip(oldGuess).map(gs => gs._2 == gs._1.getOrElse(-1))
    val numberOfConsistentPositions = consistentPositions.count(identity)
    val (newColors, oldColors) = newGuess.zip(oldGuess).filterNot(gs => gs._2 == gs._1.getOrElse(-1)).unzip
    val consistentColors = newColors.intersect(oldColors).length
    //pr(s"Comparing $newGuess with $oldGuessStats. consistentPositions: $consistentPositions")
    (numberOfConsistentPositions, consistentColors)
  }

  private def pr(p: Any, show: Boolean = false): Unit = {
    if (show) println(p)
  }
}

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

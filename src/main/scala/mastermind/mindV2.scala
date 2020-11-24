package mastermind

import scala.util.Random

object mindV2 {
  type Color = Int

  def figureItOut(balls: Int, colors: Int, duplicates: Int) = {
    val board = new Board(balls, colors, duplicates, showSolutionAtStart = false)
    //board.setSolution(Seq(2,1,1))
    val initialGuesses = Seq(((colors-balls) until colors).toList)
    val initialGuess = (0 until balls).map(_/2)
    val initialScore = board.makeGuess(initialGuess)
    var bestGuess = initialScore
    pr(s"$initialGuess $initialScore")
    while (bestGuess._1 < balls) {
      pr("")
      pr("")
      pr("")
      pr(s"Old guesses: ${board.getGuesses()}")
      val newGuess = if (board.getGuesses().length <= initialGuesses.length && bestGuess._1 < 0)
        initialGuesses(board.getGuesses().length-1)
      else {
        val newGuesses = findConsistent(balls, colors, duplicates, board.getGuesses())
        pr(newGuesses.toSet)
        val randomIndex = Random.between(0, newGuesses.length)
        newGuesses(randomIndex)
      }
      val newGuessScore = board.makeGuess(newGuess)
      if (newGuessScore._1 > bestGuess._1)
        bestGuess = newGuessScore
      pr(s"$newGuess $newGuessScore")
      //scala.io.StdIn.readLine()
    }
    pr(s"Correct after ${board.getGuesses().length} guesses")
    board.getGuesses().length
  }

  def findConsistent(balls: Int, colors: Int, duplicates: Int, oldGuesses: Seq[(Seq[Color], (Int, Int))]): Seq[Seq[Color]] = {
    val guessPosition = Seq.fill[Option[Int]](balls)(None)
    val guessColors = Seq()
    findConsistent(balls, colors, duplicates, oldGuesses, guessPosition, guessColors)
  }

  //todo add forbidden numbers
  def findConsistent(balls: Int, colors: Int, duplicates: Int, oldGuesses: Seq[(Seq[Color], (Int, Int))], guessPosition: Seq[Option[Int]], guessColors: Seq[Color]): Seq[Seq[Color]] = {
    val guessPositionString = guessPosition.map {
      case None => "*"
      case Some(x) => x.toString
    }
    pr(s"did call with: ${oldGuesses.length}, $guessPositionString, $guessColors")
    if (oldGuesses.isEmpty) {
      if (guessPosition.contains(None)) {

        val possibleColorsToAdd = if (guessColors.nonEmpty) guessColors else 0 until colors
        val newGuessColors = if (guessColors.nonEmpty) guessColors.tail else Seq()
        guessPosition.zipWithIndex.flatMap {
          case (None, ind) =>
            possibleColorsToAdd.flatMap { newColor =>
              val newGuessPosition = replace(guessPosition, index = ind, value = Some(newColor))
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
      val currentGuessConsistency = consistency(guessPosition, currentOldGuess)
      pr(s"currentGuessConsistency: $currentGuessConsistency, currentOldGuessScore: $currentOldGuessScore")
      val filterFun = (guess: Seq[Int]) => consistency(guess.map(x => Some(x)), currentOldGuess) == currentOldGuessScore

      def caller(calloldGuesses: Seq[(Seq[Int], (Int, Int))], callguessPosition: Seq[Option[Int]], callguessColors: Seq[Int]): Seq[Seq[Int]] = {
        val callguessPositionString = callguessPosition.map {
          case None => "*"
          case Some(x) => x.toString
        }
        pr(s"will call with:${calloldGuesses.length}, $callguessPositionString, $callguessColors")

        val res = findConsistent(balls, colors, duplicates, calloldGuesses, callguessPosition, callguessColors).distinct
        val filteredRes = res.filter(filterFun)
        pr(res)
        pr(s"Filter: $currentOldGuess $currentOldGuessScore Before filter: ${res.length}. After filter: ${filteredRes.length}")
        filteredRes
      }

      if (currentGuessConsistency == currentOldGuessScore) {
        caller(oldGuesses.tail, guessPosition, guessColors)
      } else if (currentGuessConsistency._1 > currentOldGuessScore._1 || currentGuessConsistency._1 + currentGuessConsistency._2 > currentOldGuessScore._1 + currentOldGuessScore._2) {
        Seq.empty[Seq[Int]]
      } else if (currentGuessConsistency._1 < currentOldGuessScore._1) {
        //add one more position guess
        pr(s"add pos: $guessPositionString")
        guessPosition.zipWithIndex.flatMap {
          case (None, ind) =>
            val addendum = currentOldGuess(ind)
            pr(s"addendum: $addendum, ind: $ind")
            val newGuessPosition = replace(guessPosition, ind, Some(addendum))
            val newGuessColors = guessColors.diff(Seq(addendum))
            caller(oldGuesses, newGuessPosition, newGuessColors)
          case (_, _) => Seq.empty[Seq[Int]]
        }
      } else if (currentGuessConsistency._2 < currentOldGuessScore._2) {
        val numberOfColorsToAdd = currentOldGuessScore._2 - currentGuessConsistency._2
        //add one more color guess
        val possibleColorsToAdd = currentOldGuess.zip(guessPosition).filter {
          case (_, None) => true
          case (oldGuess, Some(currentGuess)) => oldGuess != currentGuess
        }.map(_._1).zipWithIndex //zipWithIndex to make each element unique
        //take any numberOfColorsToAdd elements from possibleColorsToAdd
        val possibleSets = possibleColorsToAdd
          .toSet
          .subsets(numberOfColorsToAdd)
          .map(_.toSeq.map(_._1))
          .toSeq
          .distinct
        pr(s"Possible sets: $possibleSets")
        possibleSets
          .flatMap(colorsToAdd =>
            caller(oldGuesses.tail, guessPosition, guessColors.diff(colorsToAdd) ++ colorsToAdd)
          )

      } else {
        pr("This should never happen", show = true)
        Seq.empty[Seq[Int]]
        throw new RuntimeException("")
      }
    }
  }

  def replace[T](seq: Seq[T], index: Int, value: T): Seq[T] = Seq(seq.take(index), Seq(value), seq.drop(index + 1)).flatten

  def isConsistent(a: (Int, Int), b: (Int, Int)) = a._1 == b._1 && a._2 == b._2

  def consistency(newGuess: Seq[Option[Int]], oldGuess: Seq[Int]): (Int, Int) = {
    val consistentPositions = newGuess.zip(oldGuess).map(gs => gs._2 == gs._1.getOrElse(-1))
    val numberOfConsistentPositions = consistentPositions.count(identity)
    val (newColors, oldColors) = newGuess.zip(oldGuess).filterNot(gs => gs._2 == gs._1.getOrElse(-1)).unzip
    val consistentColors = newColors.flatten.intersect(oldColors).length
    //pr(s"Comparing ${newGuess.flatten} with $oldGuess. consistentPositions: ${(numberOfConsistentPositions, consistentColors)}")
    (numberOfConsistentPositions, consistentColors)
  }

  private def pr(p: Any, show: Boolean = false): Unit = {
    if (show) println(p)
  }
}

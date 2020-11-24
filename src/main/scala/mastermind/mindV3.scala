package mastermind

import scala.util.Random

object mindV3 extends mind {

  override def findConsistent(balls: Int, colors: Int, duplicates: Int, oldGuesses: Seq[(Seq[Color], (Int, Int))]): Seq[Color] = {
    val guessPosition = Seq.fill[Option[Int]](balls)(None)
    val allowedColors = Seq.fill(balls)(0 to colors)


    val newGuesses = findConsistent(balls, colors, duplicates, oldGuesses, guessPosition, allowedColors)
    pr(newGuesses.toSet)
    //val randomIndex = Math.abs((newGuesses.length-3)*(newGuesses.length-5)*(newGuesses.length-7)*(newGuesses.length-11)*(newGuesses.length-13)+17)%newGuesses.length
    val randomIndex = Random.between(0, newGuesses.length)
    newGuesses(randomIndex)
  }

  def findConsistent(balls: Int, colors: Int, duplicates: Int, oldGuesses: Seq[(Seq[Color], (Int, Int))], guessPosition: Seq[Option[Color]], allowedColors: Seq[Seq[Color]]): Seq[Seq[Color]] = {
    val guessPositionString = guessPosition.map {
      case None => "*"
      case Some(x) => x.toString
    }
    pr(s"did call with: ${oldGuesses.length}, $guessPositionString, $allowedColors")
    if (oldGuesses.isEmpty) {
      if (guessPosition.contains(None)) {

        guessPosition.zipWithIndex.find {
          case (None, _) => true
          case _ => false
        }.map {
          case (None, ind) =>
            allowedColors(ind).flatMap { newColor =>
              val newGuessPosition = replace(guessPosition, index = ind, value = Some(newColor))
              findConsistent(balls, colors, duplicates, oldGuesses, newGuessPosition, allowedColors)
            }
        }.getOrElse(Seq.empty[Seq[Color]])
      } else {
        Seq(guessPosition.flatten)
      }
    } else {
      val currentOldGuess: Seq[Color] = oldGuesses.head._1
      val currentOldGuessScore: (Int, Int) = oldGuesses.head._2
      val currentGuessConsistency = consistency(guessPosition, currentOldGuess)
      pr(s"currentGuessConsistency: $currentGuessConsistency, currentOldGuessScore: $currentOldGuessScore")
      val filterFun = (guess: Seq[Color]) => consistency(guess.map(x => Some(x)), currentOldGuess) == currentOldGuessScore

      def caller(calloldGuesses: Seq[(Seq[Color], (Int, Int))], callguessPosition: Seq[Option[Color]], callallowedColors: Seq[Seq[Color]]): Seq[Seq[Color]] = {
        val callguessPositionString = callguessPosition.map {
          case None => "*"
          case Some(x) => x.toString
        }
        pr(s"will call with:${calloldGuesses.length}, $callguessPositionString, $callallowedColors")

        val res = findConsistent(balls, colors, duplicates, calloldGuesses, callguessPosition, callallowedColors).distinct
        val filteredRes = res.filter(filterFun)
        pr(res)
        pr(s"Filter: $currentOldGuess $currentOldGuessScore Before filter: ${res.length}. After filter: ${filteredRes.length}")//, res.length > filteredRes.length)
        filteredRes
      }

      if (currentGuessConsistency._1 > currentOldGuessScore._1 || currentGuessConsistency._1 + currentGuessConsistency._2 > currentOldGuessScore._1 + currentOldGuessScore._2) {
        Seq.empty[Seq[Color]]
      } else if (currentGuessConsistency._1 == currentOldGuessScore._1) {
        val newAllowedColors = if (currentGuessConsistency._2 == currentOldGuessScore._2) {
          //all other colors should be unallowed
          val unAllowedColors = currentOldGuess.diff(guessPosition.flatten)
          removeFromEach(allowedColors, unAllowedColors)
        } else {
          allowedColors
        }.zipWithIndex.map {
          case (allowedColorsAtInd, ind) =>
            if (guessPosition(ind).isEmpty) {
              allowedColorsAtInd.diff(Seq(currentOldGuess(ind)))
            } else allowedColorsAtInd
        }
        caller(oldGuesses.tail, guessPosition, newAllowedColors)
      } else if (currentGuessConsistency._1 < currentOldGuessScore._1) {
        //add one more position guess
        pr(s"add pos: $guessPositionString")
        guessPosition.zipWithIndex.flatMap {
          case (None, ind) =>
            val addendum = currentOldGuess(ind)
            pr(s"addendum: $addendum, ind: $ind")
            val newGuessPosition = replace(guessPosition, ind, Some(addendum))
            caller(oldGuesses, newGuessPosition, allowedColors)
          case (_, _) => Seq.empty[Seq[Color]]
        }
      } else {
        pr("This should never happen", show = true)
        Seq.empty[Seq[Color]]
        throw new RuntimeException("")
      }
    }
  }
  def removeFromEach(allowedColors: Seq[Seq[Color]], removeColors: Seq[Color]): Seq[Seq[Color]] = {
    allowedColors.map(allowedColorsAtPlace => allowedColorsAtPlace.diff(removeColors))
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

}

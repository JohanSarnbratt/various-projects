package uniquedistancing

import scala.collection.mutable
import scala.collection.mutable.Seq

/**
 * from main:
 * println(Unique.possibleDistances)
 * val start = System.nanoTime()
 * Seq.range(11, 16).foreach { s =>
 * Unique.reset(s)
 * val solves = Unique.init()
 * println(s"Total solutions for $s was $solves")
 * println(s"Total time since start: ${(System.nanoTime()-start)/1000.0/1000.0/1000.0}s")
 * println("-----")
 * }
 * /*Seq.range(3, 100).foreach { s =>
 * Unique.reset(s)
 * println(s"Total edges for $s is ${s*(s-1)/2}")
 * println(s"Total unique distances: ${Unique.possibleDistances.keys.size}")
 * println("-----")
 * }*/
 */
object Unique {
  var size = 0
  var record = 0
  var showOne = true
  var possibleDistances: Map[Int, Seq[(Int, Int)]] = resetPossibleDistances()
  def resetPossibleDistances(): Map[Int, Seq[(Int, Int)]] = {
    var map = Map.empty[Int, Seq[(Int, Int)]]
    for(x <- 1 until size) {
      for(y <- 0 to x) {
        val dist = x*x+y*y
        val seq: Seq[(Int, Int)] = map.getOrElse(dist, Seq.empty[(Int, Int)]).appended((x,y))
        map = map + (dist -> seq)
      }
    }
    map
    /**
     * 1 4 9 16
     * 2x2
     * 1: (1,0)
     * 2: (1,1)
     *
     * 3x3
     * 1: (1,0)
     * 2: (1,1)
     * 4: (2,0)
     * 5: (2,1)
     * 8: (2,2)
     * 9: (3,0)
     * 10: (3,1)
     * 13: (3,2)
     * 16: (4,0)
     * 17: (4,1)
     * 18: (3,3)
     * 20: (4,2)
     * 25: (4,3) (5,0)
     * 26: (5,1)
     * 29: (5,2)
     * 32: (4,4)
     * 34: (5,3)
     * 41: (5,4)
     * 50: (5,5)
     */
  }
  def reset(s: Int): Unit = {
    Unique.size = s
    Unique.record = 0
    Unique.showOne = true
    possibleDistances = resetPossibleDistances()
  }
  def init() = {
    val board: mutable.Seq[mutable.Seq[Option[Boolean]]] = Seq.fill(size)(
      Seq.fill(size)(None)
    )
    val uniq = new Unique(board, Seq.empty, Seq.empty, 0, None, None)
    recur(uniq)
  }
  def recur(unique: Unique): Int = {
    if (unique.score == size) {
      if (showOne) {
        println("Success")
        unique.printBoard()
        showOne = false
      }
      1
    } else {
      if (unique.score >= record) {
        record = unique.score
        println(s"Record: ${unique.score}")
        unique.printBoard()
      }
      unique.possibleMoves().foldLeft(0)((sum, move) =>
        sum+recur(unique.move(move._1, move._2))
      )
    }
  }
}

class Unique(board: Seq[Seq[Option[Boolean]]], usedDistances: Seq[Int], newDistances: Seq[Int], val score: Int, newX: Option[Int], newY: Option[Int]) {
  if (newX.isDefined && newY.isDefined) {
    val xx = newX.get
    val yy = newY.get
    /*println(s"Making move $xx, $yy")
    println(s"usedDistances $usedDistances")
    println(s"newDistances $newDistances")*/
    board(xx)(yy) = Some(true)
    usedDistances.foreach { dist: Int =>
      Unique.possibleDistances(dist).foreach { dxy: (Int, Int) =>
        validateAndSetToFalse(xx+dxy._1, yy+dxy._2)
        validateAndSetToFalse(xx+dxy._2, yy+dxy._1)
        validateAndSetToFalse(xx+dxy._1, yy-dxy._2)
        validateAndSetToFalse(xx+dxy._2, yy-dxy._1)
      }
    }
    Seq.range(0, Unique.size).foreach( nx =>
      Seq.range(0, Unique.size).foreach( ny =>
        if (board(nx)(ny).getOrElse(false)) {
          newDistances.foreach { dist: Int =>
            Unique.possibleDistances(dist).foreach { dxy: (Int, Int) =>
              validateAndSetToFalse(nx+dxy._1, ny+dxy._2)
              validateAndSetToFalse(nx+dxy._2, ny+dxy._1)
              validateAndSetToFalse(nx+dxy._1, ny-dxy._2)
              validateAndSetToFalse(nx+dxy._2, ny-dxy._1)
            }
          }
        }
      )
    )
    //printBoard()
  }

  def validateAndSetToFalse(newX: Int, newY: Int): Unit = {
    if(newX >= 0 && newY >= 0 && newX < Unique.size && newY < Unique.size) {
      //require(!board(newX)(newY).getOrElse(false))
      if (board(newX)(newY).isEmpty)
        board(newX)(newY) = Some(false)
    }
  }

  def possibleMoves(): Seq[(Int, Int)] = {
    val lastX = newX.getOrElse(0)
    val lastY = newY.getOrElse(-1)
    Seq.range(lastY+1, Unique.size).flatMap(ny =>
      if (board(lastX)(ny).isEmpty) {
        val nd = nextDistances(lastX,ny)
        if (nd.distinct.length == nd.length)
          Some((lastX, ny))
        else
          None
      } else None
    ) ++
      Seq.range(lastX+1, Unique.size).flatMap( nx =>
        Seq.range(0, Unique.size).flatMap( ny =>
          if (board(nx)(ny).isEmpty) {
            val nd = nextDistances(nx,ny)
            if (nd.distinct.length == nd.length)
              Some((nx, ny))
            else
              None
          } else None
        )
      )
  }

  def nextDistances(x: Int, y: Int): mutable.Seq[Int] =
    mutable.Seq.range(0, x+1).flatMap { nx =>
      mutable.Seq.range(0, Unique.size).flatMap { ny =>
        if (board(nx)(ny).getOrElse(false))
          Some((nx-x) * (nx-x) + (ny-y) * (ny-y))
        else
          None
      }
    }

  def move(x: Int, y: Int): Unique = {
    require(board(x)(y).isEmpty)
    val nextDists = nextDistances(x,y)
    //println(nextDists)
    new Unique(board.map(_.clone), usedDistances++newDistances, nextDists, score+1, Some(x), Some(y))
  }

  def printBoard(): Unit = {
    board.foreach(
      (row: Seq[Option[Boolean]]) => {
        print("|")
        row.foreach {
          case Some(value) => if (value) print("X") else print(".")
          case _ => print(" ")
        }
        println("")
      }
    )
  }
}

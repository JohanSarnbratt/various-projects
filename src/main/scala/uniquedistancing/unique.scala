package uniquedistancing

import scala.collection.mutable.Seq

object Unique {
  val size = 3
  val possibleDistances: Map[Int, Seq[(Int, Int)]] = {
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
}

class Unique(board: Seq[Seq[Option[Boolean]]], usedDistances: Seq[Int], x: Option[Int], y: Option[Int]) {
  if(x.isDefined && y.isDefined) {
    //TODO mutate board
  }

  def move(x: Int, y: Int): Unique = {
    require(board(x)(y).isEmpty)
    val newDistances = Seq.empty //TODO
    new Unique(board, usedDistances++newDistances, Some(x), Some(y))
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

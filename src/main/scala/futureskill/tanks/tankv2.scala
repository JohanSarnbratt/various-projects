package futureskill.tanks

import scala.collection.mutable
import tanks._

/**
 * Name	Score	Time
 * Level 1 :: Target practice	471	2,611.00 ms
 * Level 2 :: First contact	1039	2,914.00 ms
 * Level 3 :: Invasion	1535	3,298.00 ms
 * Level 4 :: Hide and seek	2618	3,023.00 ms
 *
 * Total: 5663
 *
 * total: 5963 if we change line 97 to:       } else if ((goodTurns*2 >= badTurns || ticks-tickWhenCheckedoutUnknowBlock>5) && seeing(2)._1.getClass.getSimpleName == "UnknownBlock") {
 */
class Solution {
  val mapSize: Int = 42
  val map: mutable.Seq[mutable.Seq[Tile]] = mutable.Seq.fill(mapSize)(mutable.Seq.fill[Tile](mapSize)(Unknown))
  sealed trait Tile
  case object Ground extends Tile {
    override def toString = "."
  }
  case object Wall extends Tile {
    override def toString = "#"
  }
  case object Unknown extends Tile {
    override def toString = "*"
  }
  case object Target extends Tile {
    override def toString = "x"
  }
  case object Tank extends Tile {
    override def toString = "T"
  }
  //UnknownBlock can be either Target or Wall
  case class UnknownBlock(observed: Int) extends Tile {
    override def toString = "?"
  }

  var currentX: Int = mapSize/2
  var currentY: Int = mapSize/2
  //0 = east, 1 = north, 2 = west, 3 = south
  var currentDir = 1
  var ticks = 0
  var targetBehindMe = false
  var tickWhenCheckedoutUnknowBlock = 0
  var turnedToSeeTarget = false
  var goodTurns = 1
  var badTurns = 1

  def update(): Unit = {
    updateMap()
    if (API.identifyTarget()) {
      if (turnedToSeeTarget) {
        goodTurns = goodTurns + 1
      }
      API.fireCannon()
    } else {
      if (turnedToSeeTarget) {
        badTurns = badTurns + 1
      }
      val (dy,dx) = getDirVec(currentDir)
      val tileInFront = map(currentY+dy)(currentX+dx)
      val (ly,lx) = getDirVec((currentDir+1)%4)
      val tileOnLeft = map(currentY+ly)(currentX+lx)
      val (ry,rx) = getDirVec((currentDir+3)%4)
      val tileOnRight = map(currentY+ry)(currentX+rx)
      val seeing = Seq(seeTile((currentDir+1)%4),seeTile((currentDir+2)%4),seeTile((currentDir+3)%4))
      val targetBehindMeLastTime = targetBehindMe
      targetBehindMe = seeing(1)._1 == Target
      turnedToSeeTarget = false
      if (seeing.head._1 == Target) {
        turnLeft()
      } else if (seeing(2)._1 == Target) {
        turnRight()
      } else if (targetBehindMe && targetBehindMeLastTime) {
        seeing(2)._1 match {
          case UnknownBlock(_) => turnRight()
          case _ => turnLeft()
        }
      } else if ((goodTurns*2 >= badTurns || ticks-tickWhenCheckedoutUnknowBlock>20) && seeing.head._1.getClass.getSimpleName == "UnknownBlock") {
        turnLeft()
        turnedToSeeTarget = true
        tickWhenCheckedoutUnknowBlock = ticks
      } else if ((goodTurns*2 >= badTurns || ticks-tickWhenCheckedoutUnknowBlock>20) && seeing(2)._1.getClass.getSimpleName == "UnknownBlock") {
        turnRight()
        turnedToSeeTarget = true
        tickWhenCheckedoutUnknowBlock = ticks
      } else if (tileInFront == Ground) {
        moveForward()
      } else if (tileOnLeft == Ground) {
        turnLeft()
      } else {
        turnRight()
      }
    }
    ticks = ticks + 1
  }
  def seeTile(direction: Int): (Tile, Int) = {
    val (dy,dx) = getDirVec(direction)
    var len = 1
    var x = currentX+dx
    var y = currentY+dy
    var i = 1
    var seesUnknown = false
    while(x+dx >= 0 && y+dy >= 0 && x+dx < mapSize && y+dy < mapSize && map(y)(x) == Ground) {
      x = x+dx
      y = y+dy
      i = i + 1
    }
    (map(y)(x), i)
  }
  def moveForward(): Unit = {
    val (dy,dx) = getDirVec(currentDir)
    currentX = currentX + dx
    currentY = currentY + dy
    API.moveForward()
  }
  def moveBackward(): Unit = {
    val (dy,dx) = getDirVec(currentDir)
    currentX = currentX - dx
    currentY = currentY - dy
    API.moveBackward()
  }
  def turnLeft(): Unit = {
    currentDir = (currentDir+1)%4
    API.turnLeft()
  }
  def turnRight(): Unit = {
    currentDir = (currentDir+3)%4
    API.turnRight()
  }
  def updateMap(): Unit = {
    val distances = Seq(API.lidarFront(), API.lidarLeft(), API.lidarBack(), API.lidarRight())
    distances.zipWithIndex.foreach {
      case (distance, dir) =>
        val absDir = (dir+currentDir)%4
        updateLine(absDir, distance)
    }
    val (dy,dx) = getDirVec(currentDir)
    val length = distances.head
    if (API.identifyTarget()) {
      map(dy*length+currentY)(dx*length+currentX) = Target
    } else {
      map(dy*length+currentY)(dx*length+currentX) = Wall
    }
    printMap()
  }
  def updateLine(direction: Int, length: Int): Unit = {
    val (dy,dx) = getDirVec(direction)
    map(currentY)(currentX) = Tank
    (1 until length).foreach {
      l => map(dy*l+currentY)(dx*l+currentX) = Ground
    }
    val endY = dy*length+currentY
    val endX = dx*length+currentX

    map(endY)(endX) match {
      case Ground => map(endY)(endX) = Target
      case Unknown => map(endY)(endX) = UnknownBlock(1)
      case UnknownBlock(n) => map(endY)(endX) = UnknownBlock(n+1)
      case _ =>
    }
  }
  def printMap(): Unit = {
    map.foreach {
      row => println(row.mkString) // printing takes a long time and simulation may time out
    }
    println(s"fuel: ${API.currentFuel()}")
  }
  def getDirVec(direction: Int): (Int, Int) = Seq((0,1),(-1,0),(0,-1),(1,0))(direction)
}
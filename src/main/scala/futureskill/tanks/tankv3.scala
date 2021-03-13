package futureskill.tanks
import tanks._

/*
score 8649
 */
import scala.collection.mutable
class Solution3 extends SolutionInterface {
  val mapSize: Int = 42
  val map: mutable.Seq[mutable.Seq[Tile]] = mutable.Seq.fill(mapSize)(mutable.Seq.fill[Tile](mapSize)(Unknown))
  sealed trait Tile {
    def isUnknown() = this match {
      case Unknown => true
      case UnknownBlock(_) => true
      case _ => false
    }
    def isGround() = this match {
      case Ground => true
      case PatrolGround => true
      case _ => false
    }
  }
  case object Ground extends Tile {
    override def toString = "."
  }
  case object PatrolGround extends Tile {
    override def toString = ","
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
    override def toString = currentDir match {
      case 0 => ">"
      case 1 => "^"
      case 2 => "<"
      case 3 => "v"
      case _ => "T"
    }
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
  var maybeCurrentGoal = None
  var prefferedDirection = 4 // 4 no preffered
  var lastTurnWasLeft = false

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
      if (tileInFront == Wall) {
        prefferedDirection = 4
      }
      val (ly,lx) = getDirVec((currentDir+1)%4)
      val tileOnLeft = map(currentY+ly)(currentX+lx)
      val (ry,rx) = getDirVec((currentDir+3)%4)
      val tileOnRight = map(currentY+ry)(currentX+rx)
      val seeing = Seq(
        seeTile((currentDir+1)%4),
        seeTile((currentDir+2)%4),
        seeTile((currentDir+3)%4),
        seeTile(currentDir))
      println(seeing)
      val targetBehindMeLastTime = targetBehindMe
      targetBehindMe = seeing(1)._1 == Target
      turnedToSeeTarget = false
      if (seeing.head._1 == Target && seeing(2)._1 == Target) {
        if (lastTurnWasLeft)
          turnLeft()
        else
          turnRight()
      } else if (seeing.head._1 == Target) {
        turnLeft()
      } else if (seeing(2)._1 == Target) {
        turnRight()
      } else if (targetBehindMe && targetBehindMeLastTime) {
        if (seeing(2)._1.isUnknown())
          turnRight()
        else if (seeing.head._1.isUnknown())
          turnLeft()
        else if (seeing(2)._3)
          turnRight()
        else
          turnLeft()
      } else if (
        seeing.head._1.isUnknown() && goodTurns > badTurns/2 && ticks > 100
          || seeing.head._3 && !seeing(3)._3) {
        turnLeft()
        turnedToSeeTarget = true
      } else if (
        seeing(2)._1.isUnknown() && goodTurns > badTurns/2 && ticks > 100
          || seeing(2)._3 && !seeing(3)._3) {
        turnRight()
        turnedToSeeTarget = true
      } else if (tileInFront.isGround() && (prefferedDirection == currentDir || prefferedDirection > 3 || seeing(3)._3)) {
        moveForward()
        prefferedDirection = currentDir
      } else {
        val leftScore = Seq(
            (currentDir+1)%4 == prefferedDirection,
            seeing.head._1.isUnknown(),
            seeing.head._2 > seeing(2)._2,
            lastTurnWasLeft
          ).count(x => x)
        val rightScore = Seq(
          (currentDir+3)%4 == prefferedDirection,
          seeing(2)._1.isUnknown(),
          seeing(2)._2 > seeing.head._2,
          !lastTurnWasLeft
        ).count(x => x)
        if (leftScore >= rightScore) {
          turnLeft()
        } else {
          turnRight()
        }
      }
    }
    ticks = ticks + 1
  }
  def seeTile(direction: Int): (Tile, Int, Boolean) = {
    val (dy,dx) = getDirVec(direction)
    var seesPatrolGround = false
    var len = 1
    var x = currentX+dx
    var y = currentY+dy
    var i = 1
    var seesUnknown = false
    while(x+dx >= 0 && y+dy >= 0 && x+dx < mapSize && y+dy < mapSize && map(y)(x).isGround()) {
      if (map(y)(x) == PatrolGround) {
        seesPatrolGround  = true
      }
      x = x+dx
      y = y+dy
      i = i + 1
    }
    (map(y)(x), i, seesPatrolGround)
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
    lastTurnWasLeft = true
    API.turnLeft()
  }
  def turnRight(): Unit = {
    currentDir = (currentDir+3)%4
    lastTurnWasLeft = false
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
      l =>
        map(dy*l+currentY)(dx*l+currentX) match {
          case Target => map(dy*l+currentY)(dx*l+currentX) = PatrolGround
          case UnknownBlock(_) => map(dy*l+currentY)(dx*l+currentX) = PatrolGround
          case PatrolGround => map(dy*l+currentY)(dx*l+currentX) = PatrolGround
          case _ => map(dy*l+currentY)(dx*l+currentX) = Ground
        }
    }
    val endY = dy*length+currentY
    val endX = dx*length+currentX

    map(endY)(endX) match {
      case Ground => map(endY)(endX) = Target
      case Tank => map(endY)(endX) = Target
      case Unknown => map(endY)(endX) = UnknownBlock(1)
      case UnknownBlock(n) => map(endY)(endX) = UnknownBlock(n+1)
      case _ =>
    }
  }
  def printMap(): Unit = {
    /*map.foreach {
      row =>
      if (row.exists(tile => tile != Unknown))
        println(row.mkString) // printing takes a long time and simulation may time out
    }*/
    println(s"fuel: ${API.currentFuel()}")
  }
  def getDirVec(direction: Int): (Int, Int) = Seq((0,1),(-1,0),(0,-1),(1,0))(direction)
}
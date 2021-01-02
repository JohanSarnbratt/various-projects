package futureskill.tanks

object tanks {

  trait SolutionInterface {
    def update(): Unit
  }

  trait Api {
    def identifyTarget(): Boolean

    def fireCannon(): Unit

    def currentFuel(): Int

    def moveForward(): Unit

    def moveBackward(): Unit

    def turnLeft(): Unit

    def turnRight(): Unit

    def lidarFront(): Int

    def lidarLeft(): Int

    def lidarBack(): Int

    def lidarRight(): Int
  }

  val API: Api = ???
}
object elevator {

  trait SolutionInterface {
    def update(): Unit
  }

  trait Api {

    def openDoor(): Unit
    def moveUp(): Unit
    def moveDown(): Unit
    def getFloorCount(): Int
    def getElevatorCapacity(): Int
    def getCurrWeight(): Int
    def getUpBtnStatus(floor: Int): Boolean
    def getDownBtnStatus(floor: Int): Boolean
    def getBtnPressedStatus(floor: Int): Boolean
  }

  val API: Api = ???
}
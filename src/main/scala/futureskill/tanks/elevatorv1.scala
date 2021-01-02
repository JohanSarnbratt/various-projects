package futureskill.tanks
import elevator._

/**
 * v1
 * rating: 1238
 * place: 13/49
 *
 */
class SolutionV1 extends SolutionInterface {
  // You can initiate and calculate things here
  var onTheWayUp = true
  var pickUpPassengers = true
  var currentFloor = 0
  val totalFloors = API.getFloorCount()
  var handledDoor = false

  /**
   * Executes a single step of the elevators programming. This is where you would
   * typically call all API functions.
   */
  def update(): Unit = {
    if (!handledDoor) {
      val tookAction = handleDoor()
      if (!tookAction)
        moveFloor()
    } else {
      moveFloor()
    }
  }
  def handleDoor() = {
    val spaceLeft = API.getElevatorCapacity() - API.getCurrWeight()
    println(s"spaceLeft $spaceLeft")
    val availablePassangers =
      if (API.getCurrWeight() == 0)
        API.getUpBtnStatus(currentFloor) || API.getDownBtnStatus(currentFloor)
      else if (onTheWayUp)
        API.getUpBtnStatus(currentFloor)
      else
        API.getDownBtnStatus(currentFloor)
    println(s"availablePassangers $availablePassangers")
    val passengersWantOff = API.getBtnPressedStatus(currentFloor)
    println(s"passengersWantOff $passengersWantOff")
    val openDoor = spaceLeft >= 100 && availablePassangers || passengersWantOff
    if (openDoor) {
      println(s"opening door...")
      API.openDoor()
    }
    handledDoor = true
    openDoor
  }
  def moveFloor() = {
    val newSpaceLeft = API.getElevatorCapacity() - API.getCurrWeight()
    println(s"newSpaceLeft $newSpaceLeft")
    println(s"Buttons: ${(0 until totalFloors).map(f => API.getBtnPressedStatus(f))}")
    val passengerKeepDirection =
      if (onTheWayUp)
        (currentFloor+1 until totalFloors).exists(f => API.getBtnPressedStatus(f))
      else
        (0 until currentFloor).exists(f => API.getBtnPressedStatus(f))
    println(s"passengerKeepDirection $passengerKeepDirection")
    val emptyElevatorKeepDirection = API.getCurrWeight() == 0 && {
      val maybeFirstPassengerAbove =
        (currentFloor+1 until totalFloors)
          .find(f => API.getUpBtnStatus(f) || API.getDownBtnStatus(f))
          .map(i => i - currentFloor)
      val maybeFirstPassengerBelow =
        (0 until currentFloor)
          .reverse
          .find(f => API.getUpBtnStatus(f) || API.getDownBtnStatus(f))
          .map(i => currentFloor - i)
      if (onTheWayUp)
        maybeFirstPassengerAbove
          .map(a => maybeFirstPassengerBelow.map(b => a <= b).getOrElse(true))
          .getOrElse(false)
      else
        maybeFirstPassengerBelow
          .map(b => maybeFirstPassengerAbove.map(a => b <= a).getOrElse(true))
          .getOrElse(false)
    }
    println(s"emptyElevatorKeepDirection $emptyElevatorKeepDirection")
    if (!passengerKeepDirection && !emptyElevatorKeepDirection)
      onTheWayUp = !onTheWayUp
    println(s"onTheWayUp $onTheWayUp")
    if (onTheWayUp)
      goUp()
    else
      goDown()
    handledDoor = false
  }
  def goUp() = {
    println("Going up")
    if (API.getFloorCount() > currentFloor + 1)
      currentFloor = currentFloor + 1
    API.moveUp()
  }
  def goDown() = {
    println("Going down")
    if (0 < currentFloor)
      currentFloor = currentFloor - 1
    API.moveDown()
  }
}

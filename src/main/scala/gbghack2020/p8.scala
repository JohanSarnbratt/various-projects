package gbghack2020

object p8 {
  def run() = {
    val data = readData()
    //val take = 4
    //simulate(data._1.take(take).iterator, data._2.take(take).iterator, data._3.take(take).iterator)
    simulate(data._1.iterator, data._2.iterator, data._3.iterator)
  }
  case class Log(floor: Int, timeOnFloor: Int)
  case class State(floor: Int, startTime: Int, endTime: Int)
  private def gen(seq: Seq[Int]): Log = Log(seq.head, seq(1))

  def readData() = {

    val bufferedSource = io.Source.fromFile("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/gbghack2020/p8data")
    var rows: (Seq[Log],Seq[Log],Seq[Log]) = (Seq.empty,Seq.empty,Seq.empty)
    for (line <- bufferedSource.getLines) {
      val cols: Seq[Int] = line.split(",").map(_.trim).map(Integer.parseInt)
      require(cols.length == 6)
      rows = (rows._1.appended(gen(cols.take(2))), rows._2.appended(gen(cols.slice(2, 4))), rows._3.appended(gen(cols.drop(4))))
    }
    rows
  }
  def simulate(e0: Iterator[Log], e1: Iterator[Log], e2: Iterator[Log]) = {
    var currentState: Seq[State] = Seq(nextState(None, e0.next(), true), nextState(None, e1.next(), true), nextState(None, e2.next(), true))
    var numberOfNotLonely = 0
    val elevators = Seq(e0, e1, e2)
    while (e1.hasNext || e2.hasNext || e0.hasNext) {
      val nextElevator: Int = currentState.zipWithIndex.minBy(_._1.endTime)._2
      val leavingFloor = currentState(nextElevator).floor
      val elevatorsAtThisFloor = currentState.count(_.floor == leavingFloor)
      if (elevatorsAtThisFloor == 2) {
        numberOfNotLonely = numberOfNotLonely + 1
        println("ding")
        println(s"nextElevator: $nextElevator leavingFloor: $leavingFloor")
        if (currentState.exists(_.startTime == currentState(nextElevator).endTime)) println(currentState)
      }
      val nextLog = elevators(nextElevator).next()
      val moreRides = elevators(nextElevator).hasNext
      currentState = newStates(currentState, nextElevator, nextLog, moreRides)
    }
    println(s"numberOfNotLonely $numberOfNotLonely")
  }
  def newStates(states: Seq[State], movingElevator: Int, log: Log, moreRides: Boolean) = {
    val nexState = nextState(Some(states(movingElevator)), log, moreRides)
    //println(s"Elevator $movingElevator moving from ${states(movingElevator).floor} to ${nexState.floor} at ${states(movingElevator).endTime}")
    states.take(movingElevator) ++ Seq(nexState) ++ states.drop(movingElevator+1)
  }
  def nextState(currentState: Option[State], nextLog: Log, moreRides: Boolean) = {
    currentState match {
      case None => State(nextLog.floor, 0, nextLog.timeOnFloor)
      case Some(oldState) =>
        val travelTime = if (oldState.floor > nextLog.floor)
          2*(oldState.floor - nextLog.floor)
        else
          3*(nextLog.floor - oldState.floor)
        val stayForever = if (moreRides) 0 else 2345632
        State(nextLog.floor, oldState.endTime+travelTime, oldState.endTime+travelTime+nextLog.timeOnFloor-1+stayForever)
    }
  }
}

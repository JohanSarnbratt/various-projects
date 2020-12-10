package aoc2019

object Day7 {
  def run() = {
    val testPrograms = Seq(
      Seq(3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0),
      Seq(3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0),
      Seq(3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0)
    )
    val testResutls = Seq(43210, 54321, 65210)
    testPrograms.zip(testResutls).foreach { test =>
      val signal = getBestSignal(test._1)
      println(signal == test._2)
    }
    //Part 1:
    val puzzleInput = Seq(3, 8, 1001, 8, 10, 8, 105, 1, 0, 0, 21, 34, 51, 64, 73, 98, 179, 260, 341, 422, 99999, 3, 9, 102, 4, 9, 9, 1001, 9, 4, 9, 4, 9, 99, 3, 9, 1001, 9, 4, 9, 1002, 9, 3, 9, 1001, 9, 5, 9, 4, 9, 99, 3, 9, 101, 5, 9, 9, 102, 5, 9, 9, 4, 9, 99, 3, 9, 101, 5, 9, 9, 4, 9, 99, 3, 9, 1002, 9, 5, 9, 1001, 9, 3, 9, 102, 2, 9, 9, 101, 5, 9, 9, 1002, 9, 2, 9, 4, 9, 99, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 99, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 99, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 99, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 99, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 99)
    println(getBestSignal(puzzleInput))
    //part 2:
    println(getBestSignal2(Seq(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
      27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)) == 139629729)
    println(getBestSignal2(Seq(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
      -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
      53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10)) == 18216)
    println(getBestSignal2(puzzleInput))

  }

  def getBestSignal(program: Seq[Int]): Int = {
    val thrusterSettings = Seq(4, 3, 2, 1, 0).permutations.toSeq
    val bestSetting = thrusterSettings.maxBy(getSignal(program, _))
    getSignal(program, bestSetting)
  }

  def getSignal(program: Seq[Int], thrusterSetting: Seq[Int]) = {
    var signal = 0
    thrusterSetting.foreach { ts =>
      val icr = new IntCodeRunner(program)
      icr.run(Seq(ts, signal))
      signal = icr.getOutput.head
    }
    signal
  }

  def getBestSignal2(program: Seq[Int]): Int = {
    val thrusterSettings = Seq(5,6,7,8,9).permutations.toSeq
    val bestSetting = thrusterSettings.maxBy(getSignal2(program, _))
    getSignal2(program, bestSetting)
  }
  def getSignal2(program: Seq[Int], thrusterSetting: Seq[Int]) = {
    val icrs = Seq(new IntCodeRunner(program), new IntCodeRunner(program), new IntCodeRunner(program), new IntCodeRunner(program), new IntCodeRunner(program))
    var signal = 0
    var status: IntCodeRunner.State = IntCodeRunner.Running
    icrs.zip(thrusterSetting).foreach { case (icr, ts) =>
      status = icr.run(Seq(ts, signal))
      signal = icr.getOutput.last
    }
    while (status != IntCodeRunner.Halted) {
      icrs.foreach { case icr =>
        status = icr.run(Seq(signal))
        signal = icr.getOutput.last
      }
    }
    signal
  }
}

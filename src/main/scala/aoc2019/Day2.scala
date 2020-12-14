package aoc2019

object Day2 {
  def run() = {
    var icr = new IntCodeRunner(Seq(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50))
    icr.run()
    println(icr.getProgram == Seq(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50))
    icr = new IntCodeRunner(Seq(1, 0, 0, 0, 99))
    icr.run()
    println(icr.getProgram == Seq(2, 0, 0, 0, 99))

    //todo broken tests because of new typ of run()
    //println(new IntCodeRunner(Seq(2, 3, 0, 3, 99)).run() == Seq(2, 3, 0, 6, 99))
    //println(new IntCodeRunner(Seq(2, 4, 4, 5, 99, 0)).run() == Seq(2, 4, 4, 5, 99, 9801))
    //println(new IntCodeRunner(Seq(1, 1, 1, 4, 99, 5, 6, 0, 99)).run() == Seq(30, 1, 1, 4, 2, 5, 6, 0, 99))
    /*val data = Seq(1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,5,19,23,2,9,23,27,1,27,5,31,2,31,13,35,1,35,9,39,1,39,10,43,2,43,9,47,1,47,5,51,2,13,51,55,1,9,55,59,1,5,59,63,2,6,63,67,1,5,67,71,1,6,71,75,2,9,75,79,1,79,13,83,1,83,13,87,1,87,5,91,1,6,91,95,2,95,13,99,2,13,99,103,1,5,103,107,1,107,10,111,1,111,13,115,1,10,115,119,1,9,119,123,2,6,123,127,1,5,127,131,2,6,131,135,1,135,2,139,1,139,9,0,99,2,14,0,0)
    val icr1 = new IntCodeRunner(data)
    icr1.run()
    val res1 = icr1.getProgram
    println(res1.head)
    (0 until 10000).foreach(nounverb => {
      val noun = nounverb/100
      val verb = nounverb%100
      val newData = data.head :: noun :: verb :: data.drop(3).toList
      val newIcr = new IntCodeRunner(newData)
      newIcr.run()
      val res = newIcr.getProgram
      if (res.head == 19690720) {
        println(nounverb)
      }
    })*/
  }
}

package aoc2020

object Day13 {

  def run() = { //part 1
    val data = AocHelpers.readLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data13")
    val timestamp = Integer.parseInt(data.head)
    val buses = data.last.split(',').flatMap {
      case "x" => None
      case number => Some(Integer.parseInt(number))
    }
    val bus = buses.minBy(b => b - timestamp % b)
    println(s"$bus ${bus - timestamp % bus} ${bus*(bus - timestamp % bus)}")

    println(part2("7,13,x,x,59,x,31,19") == 1068781)
    println(part2("17,x,13,19") == 3417)
    println(part2("67,7,59,61") == 754018)
    println(part2("67,x,7,59,61") == 779210)
    println(part2("67,7,x,59,61") == 1261476)
    println(part2("1789,37,47,1889") == 1202161486)
    println(part2(data.last))

  }
  def part2(busses: String) = {

    val timestamp2 = busses.split(',').map {
      case "x" => None
      case number => Some(number.toLong)
    }.toSeq
    val busesWithOffset: Seq[(Long, Long)] = timestamp2.zipWithIndex.filter {
      case (Some(_), _) => true
      case _ => false
    }.map{
      case (Some(bus), ind) => (bus, ind.toLong)
    }
    val res: (Long, Long) = busesWithOffset.tail.fold(busesWithOffset.head) {
      case ((bus1, offsets1), (bus2, offsets2)) =>
        //println((bus1, offsets1), (bus2, offsets2))
        val newId = bus1*bus2
        var i1 = 0L
        var i2 = 0L
        def bus1Time = i1*bus1 + offsets1
        def bus2Time = i2*bus2 + offsets2
        while (bus1Time != bus2Time) {
          if (bus1Time < bus2Time) {
            i1 = i1 + Math.max((bus2Time - bus1Time)/bus1, 1)
          } else {
            i2 = i2 + Math.max((bus1Time - bus2Time)/bus2, 1)
          }
        }
        val r = (newId, bus1Time)
        //println((i1, i2))
        //println(r)
        r
    }
    //println(timestamp2)
    val t = res._1-res._2
    //println(t)
    //println(timestamp2.zipWithIndex.map{case (maybeBus, ind) => maybeBus.map((t+ind.toLong) % _)})
    //println(timestamp2.zipWithIndex.map{case (maybeBus, ind) => maybeBus.map((1068781+ind.toLong) % _)})
    t
  }
}
 // t % b0 == 0, (t+1) % b1 == 0, ...
// b0 * n ==  b1 * m + 1 == b2 *
/*

t = 19n   = 19*41q + 19k    , k<41
t = 41m-9 = 19*41r + 41p - 9, p<19

19n = 41m-9
n = 41/19 m - 9/19


t % 19 == 0
t+9 % 41 == 0
t+19 % 643 == 0

t % (19*41) == 0,19,38,...
t+9 % (19*41) == 0,41,82,...
t % (19*41) == 32,73,114,...

643*509

509 5 2545
(779,List(665))
(500897,List(122189))
(8515249,List(623086))
(110698237,List(9138335))

509 5 2545
(779,List(665))
(500897,List(122189))
(8515249,List(623086))
(110698237,List(9138335))
(2546059451,List(1337517179))
(1295944260559,List(760063233577))

 */
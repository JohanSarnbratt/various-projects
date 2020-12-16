package aoc2020

import scala.collection.mutable

object Day16 {

  def run() = { //part 1
    val dataTest1: Seq[Seq[String]] = AocHelpers.readDataGroupsSeparatedByBlankLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data16test")
    val data = AocHelpers.readDataGroupsSeparatedByBlankLines("/Users/johansarnbratt/personal/trianglepeg/src/main/scala/aoc2020/data16")
    println(getRanges(dataTest1.head))
    println(getTickets(dataTest1(2).tail))
    println(sumInvalidNumbers(dataTest1))
    println(sumInvalidNumbers(data))
    println(figureOut(dataTest1))
    val figured = figureOut(data)
    println(figured)
    val mahTicket = getTickets(data(1).tail).head
    //The 6 first fields start with departure-
    val indices = figured.zipWithIndex.sortBy(_._1).map(_._2).take(6)
    println(indices)
    println(indices.map(mahTicket(_)))
    println(indices.map(mahTicket(_)).map(_.toLong).product)

  }
  def getRanges(seqs: Seq[String]): Seq[Seq[(Int, Int)]] = {
    seqs.map ( _.split(": ").last.split(" or ").toSeq.map(_.split("-").map(Integer.parseInt)).map(arr => (arr(0), arr(1))))
  }
  def getTickets(seqs: Seq[String]): Seq[Seq[Int]] = {
    seqs.map(_.split(",").toSeq.map(Integer.parseInt))
  }
  def figureOut(data: Seq[Seq[String]]) = {
    val ranges: Seq[Seq[(Int, Int)]] = getRanges(data.head)
    val tickets = getTickets(data(2).tail)
    val validTickets = filterInvalidTickets(tickets, ranges)
    val initPosToFieldPossibilities: Seq[Seq[Int]] = Seq.fill(tickets.length)(ranges.indices)
    val validFields = validTickets.foldLeft(initPosToFieldPossibilities) {
      case (posToField, ticket) =>
        posToField.zip(ticket).map {
          case (possibleRanges, ticketVal) =>
            possibleRanges.filter(ind => ranges(ind).exists{
              case (low, high) => low <= ticketVal && ticketVal <= high
            })
        }
    }
    validFields.zipWithIndex.sortBy(_._1.length).foldLeft(Seq[(Int, Int)]()) {
      case (finalValids, potentialValids) =>
        finalValids.appended(potentialValids._1.filterNot(finalValids.map(_._1).contains(_)).head, potentialValids._2)
    }.sortBy(_._2).map(_._1)
  }
  def filterInvalidTickets(tickets: Seq[Seq[Int]], ranges: Seq[Seq[(Int, Int)]]) = {
    tickets.filter {
      ticket: Seq[Int] => ticket.forall {
        num =>
          ranges.exists(_.exists {
            case (low, high) =>
              //println(s"num: $num is in this range: ${low <= num && num <= high}")
              low <= num && num <= high
          })
      }
    }
  }
  def sumInvalidNumbers(data: Seq[Seq[String]]) = {
    val ranges: Seq[Seq[(Int, Int)]] = getRanges(data.head)
    val tickets = getTickets(data(2).tail)
    tickets.flatMap {
      ticket => ticket.filterNot {
        num =>
          ranges.exists(_.exists {
            case (low, high) =>
              //println(s"num: $num is in this range: ${low <= num && num <= high}")
              low <= num && num <= high
          })
      }
    }.sum
  }
}
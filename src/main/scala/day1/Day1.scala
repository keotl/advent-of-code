package day1
import utils._
import scala.annotation.tailrec

def day1(input: String): Int = {
  val (a, b) = parseInput(input)
  a.lazyZip(b)
    .map(_ - _)
    .map(Math.abs)
    .sum
}

private def parseInput(input: String): (Seq[Int], Seq[Int]) = {
  val rows = input
    .split("\n")
    .map(cleanupWhitespace)
    .map(_.split(" "))

  val a = rows.map(_(0).toInt)
  val b = rows.map(_(1).toInt)
  // Can be improved with radix-sort to O(n)
  (a.sorted, b.sorted)
}

private val pattern = "(\\W+)".r
private def cleanupWhitespace(x: String): String = {
  pattern.replaceAllIn(x, " ")
}

def day1b(input: String): Int = {
  val (a, b) = parseInput(input)

  val occurrences = countOccurrences(List(b*), Map())

  a
    .map(a => occurrences.getOrElse(a, 0) * a)
    .sum
}

@tailrec private def countOccurrences(
    elements: Seq[Int],
    accumulator: Map[Int, Int]
): Map[Int, Int] = {
  elements match {
    case Nil => accumulator
    case e :: tail => {
      val occurrences = accumulator.getOrElse(e, 0)
      val updated = accumulator + (e -> (occurrences + 1))
      countOccurrences(tail, updated)
    }
  }

}

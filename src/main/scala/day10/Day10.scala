package day10

import scala.annotation.tailrec

def day10a(input: String): Int = {
  val map = parseMap(input)

  // println(map.map(_.toSeq).toSeq.mkString("\n"))
  val startPositions = findStartPositions(map)

  // println(startPositions)

  startPositions
    .map(reachableSummits(map))
    .map(_.size)
    .sum
}

def day10b(input: String): Int = {
  val map = parseMap(input)

  // println(map.map(_.toSeq).toSeq.mkString("\n"))
  val startPositions = findStartPositions(map)

  // println(startPositions)

  startPositions
    .map(computeTrailheadRating(map))
    .sum
}

private def parseMap(input: String): Array[Array[Int]] = {
  val rows = input.split("\n")

  rows
    .map(_.map(c => if (c == '.') then (-1) else c.toString.toInt).toArray)
}

private def reachableSummits(
    map: Array[Array[Int]]
)(startPos: (Int, Int)): Set[(Int, Int)] = {
  val (i, j) = startPos
  val currentElevation = map(i)(j)
  // println(s"Counting from ${startPos}, currentElevation: ${currentElevation}")

  if (currentElevation == 9) {
    return Set((i, j))
  }

  neighbours(i, j)
    .filter(isInsideMap(map))
    .filter(pos => map(pos._1)(pos._2) == currentElevation + 1)
    .flatMap(reachableSummits(map))
    .toSet

}

private def computeTrailheadRating(
    map: Array[Array[Int]]
)(startPos: (Int, Int)): Int = {
  val (i, j) = startPos
  val currentElevation = map(i)(j)
  // println(s"Counting from ${startPos}, currentElevation: ${currentElevation}")

  if (currentElevation == 9) {
    return 1
  }

  neighbours(i, j)
    .filter(isInsideMap(map))
    .filter(pos => map(pos._1)(pos._2) == currentElevation + 1)
    .map(computeTrailheadRating(map))
    .sum

}

private def neighbours(i: Int, j: Int): Seq[(Int, Int)] = {
  Seq(
    (i + 1, j),
    (i - 1, j),
    (i, j + 1),
    (i, j - 1)
  )
}

private def isInsideMap[T](map: Array[Array[T]])(pos: (Int, Int)): Boolean = {
  pos._1 >= 0 && pos._1 < map.length && pos._2 >= 0 && pos._2 < map(0).length
}

private def findStartPositions(map: Array[Array[Int]]): Seq[(Int, Int)] = {
  map
    .map(_.zipWithIndex.filter((e, _) => e == 0).map((_, j) => j))
    .zipWithIndex
    .flatMap((js, i) => js.map((i, _)))
}

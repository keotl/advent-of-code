package day12

import scala.collection.mutable.Queue

def day12a(input: String): Long = {
  val map = parseMap(input)
  val regions = computeContiguousRegions(map)

  regions
    .map(calculateCost)
    .sum
}

private def parseMap(input: String): Array[Array[Char]] = {
  val rows = input.split("\n")

  rows
    .map(_.toArray)
}

private case class Region(name: Char, points: Set[(Int, Int)])

private def computeContiguousRegions(map: Array[Array[Char]]): Seq[Region] = {
  var regions = Seq[Region]()

  for (
    i <- Range(0, map.length);
    j <- Range(0, map(0).length)
  ) {
    if (regions.find(r => r.points.contains((i, j))).isEmpty) {
      regions = regions :+ findLargestContiguousAtPoint(map)((i, j))
    }
  }

  regions
}

private def findLargestContiguousAtPoint(
    map: Array[Array[Char]]
)(pos: (Int, Int)): Region = {
  val (i, j) = pos
  val name = map(i)(j)

  var seen = Set[(Int, Int)](pos)
  val queue = Queue(pos)
  while (queue.nonEmpty) {
    val (i, j) = queue.dequeue()

    for (
      neighbour <- neighbours(i, j)
        .filter(isInsideMap(map))
        .filter(n => !seen.contains(n))
    ) {
      if (map(neighbour._1)(neighbour._2) == name) {
        queue.enqueue(neighbour)
        seen = seen + neighbour
      }
    }
  }

  Region(name, seen)
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

private def calculateCost(region: Region): Int = {
  val area = region.points.size
  def perimeterUnitCost(i: Int, j: Int): Int = {
    val insideNeighbours = neighbours(i, j)
      .filter(region.points.contains)
      .length

    4 - insideNeighbours
  }

  val perimeter = region.points.toSeq.map(perimeterUnitCost).sum

  return area * perimeter
}

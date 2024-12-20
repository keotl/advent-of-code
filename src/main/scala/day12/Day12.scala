package day12

import scala.collection.mutable.Queue

def day12a(input: String): Long = {
  val map = parseMap(input)
  val regions = computeContiguousRegions(map)

  regions
    .map(calculateCost)
    .sum
}

def day12b(input: String): Long = {
  val map = parseMap(input)
  val regions = computeContiguousRegions(map)
  // println(map.map(_.toSeq).mkString("\n"))
  regions
    .map(calculateCostContinuousSides)
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

private def calculateCostContinuousSides(region: Region): Int = {
  val area = region.points.size
  val corners = countCorners(region)

  return area * corners
}

private def countCorners(region: Region): Int = {
  val corners = region.points
    .flatMap(findCorners(region))
    .toSet

  corners.size
}

private def findCorners(region: Region)(tile: (Int, Int)): Set[(Int, Int)] = {
  val (i, j) = tile
  val candidates = Seq((i, j), (i + 1, j), (i, j + 1), (i + 1, j + 1))

  var result: Set[(Int, Int)] = Set()

  for (candidate <- candidates) {
    isCorner(region)(candidate._1, candidate._2) match {
      case 1 => result = result + candidate
      case 2 =>
        result = result + candidate + ((
          candidate._1 * 1000000, // Janky way to count a corner twice when it is a diagonal inside region
          candidate._2 * 1000000
        ))
      case _ => ()
    }
  }

  result
}

// Convention: corner (i,j) is top-left of tile (i,j)
private def isCorner(region: Region)(i: Int, j: Int): Int = {
  val up = region.points.contains((i - 1, j))

  val insideNeighbours = Seq((i - 1, j - 1), (i - 1, j), (i, j - 1), (i, j))
    .filter(region.points.contains)

  if (insideNeighbours.length % 4 == 0) {
    return 0
  }

  if (insideNeighbours.length % 2 == 1) {
    return 1
  }

  val a = insideNeighbours(0)
  val b = insideNeighbours(1)

  val di = b._1 - a._1
  val dj = b._2 - a._2
  if (di == 0 || dj == 0) {
    // 2 adjacent, does not yield corner
    return 0
  }

  2
}

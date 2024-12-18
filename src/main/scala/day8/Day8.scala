package day8

import scala.annotation.tailrec

def day8a(input: String): Int = {
  val rows = input.split("\n")

  val nodeGroups = parseInput(rows)
  val mapSize = (rows.length, rows.head.length)

  nodeGroups
    .map(_._2)
    .flatMap(findAntiNodes(mapSize))
    .toSet
    .size
}

def day8b(input: String): Int = {
  val rows = input.split("\n")

  val nodeGroups = parseInput(rows)
  val mapSize = (rows.length, rows.head.length)

  nodeGroups
    .map(_._2)
    .filter(_.length > 1)
    .flatMap(findAntiNodes(mapSize, getAntiNodeCandidatesAllAligned(mapSize)))
    .toSet
    .size
}

private def parseInput(rows: Seq[String]): Seq[(Char, Seq[(Int, Int)])] = {
  val parsed = rows.zipWithIndex
    .map((row, i) => (row.toSeq.zipWithIndex.filter(_._1 != '.'), i))
    .filter(_._1.length > 0)
    .flatMap((row, i) => row.map((letter, j) => (letter, i, j)))
    .groupBy(_._1)
    .view
    .mapValues(_.map(x => (x._2, x._3)))

  parsed.toSeq
}

private def findAntiNodes(
    mapSize: (Int, Int),
    candidateFactory: (a: (Int, Int), b: (Int, Int)) => Seq[(Int, Int)] =
      getAntiNodeCandidatePair
)(nodes: Seq[(Int, Int)]): Seq[(Int, Int)] = {
  val nodePairs = for {
    a <- nodes.zipWithIndex
    b <- nodes.slice(a._2 + 1, nodes.length)
  } yield (a._1, b)

  nodePairs
    .flatMap(candidateFactory.tupled)
    .filter(isWithinMap(mapSize))
}

private def getAntiNodeCandidatePair(
    a: (Int, Int),
    b: (Int, Int)
): Seq[(Int, Int)] = {
  val di = b._1 - a._1
  val dj = b._2 - a._2

  Seq(
    (a._1 - di, a._2 - dj),
    (b._1 + di, b._2 + dj)
  )
}

private def getAntiNodeCandidatesAllAligned(mapSize: (Int, Int))(
    a: (Int, Int),
    b: (Int, Int)
): Seq[(Int, Int)] = {
  val di = b._1 - a._1
  val dj = b._2 - a._2

  createAlignedPositions(mapSize)(
    (di, dj),
    b,
    List(b)
  ) ++ createAlignedPositions(
    mapSize
  )((-di, -dj), a, List(a))
}

@tailrec private def createAlignedPositions(mapSize: (Int, Int))(
    delta: (Int, Int),
    pos: (Int, Int),
    acc: List[(Int, Int)]
): Seq[(Int, Int)] = {
  val candidate = (pos._1 + delta._1, pos._2 + delta._2)
  if (!isWithinMap(mapSize)(candidate)) {
    acc
  } else {
    createAlignedPositions(mapSize)(delta, candidate, candidate :: acc)
  }

}

private def isWithinMap(mapSize: (Int, Int))(pos: (Int, Int)): Boolean = {
  (pos._1 >= 0 && pos._1 < mapSize._1) && (pos._2 >= 0 && pos._2 < mapSize._2)
}

package day6

import utils.|>
import scala.annotation.tailrec

def day6a(input: String): Int = {
  val obstacleMap = parseObstacleMap(input.split("\n"))
  val guardPos = findGuardPosition(input.split("\n"))

  val guardState = createGuardPatrolState(obstacleMap, guardPos, "n")
    |> simulateGuardPatrol(obstacleMap)

  guardState.numberVisited
}

private def parseObstacleMap(lines: Seq[String]): Array[Array[Boolean]] = {
  lines
    .map(_.map(_ == '#').toArray)
    .toArray
}

private def findGuardPosition(lines: Seq[String]): (Int, Int) = {
  val matchingCols = lines
    .map(
      _.zipWithIndex
        .find(_._1 == '^')
        .map(_._2)
    )

  val matchingRow = matchingCols.zipWithIndex
    .find(_._1.nonEmpty)

  (matchingRow.get._2, matchingRow.get._1.get)
}

type Heading = "n" | "e" | "s" | "w"

private case class GuardPatrolState(
    visited: Array[Array[Boolean]],
    numberVisited: Int,
    position: (Int, Int),
    heading: Heading
)

@tailrec private def simulateGuardPatrol(obstacles: Array[Array[Boolean]])(
    state: GuardPatrolState
): GuardPatrolState = {
  val mapSize = (obstacles.length, obstacles(0).length)
  val destination = nextTile(state.position, state.heading)

  // println(s"${state.position},${state.heading} -> ${destination}")

  if (isOutOfBounds(mapSize)(destination)) {
    return state
  }

  if (obstacles(destination._1)(destination._2)) {
    return simulateGuardPatrol(obstacles)(
      state.copy(heading = rotate(state.heading))
    )
  }

  val updatedState = visit(destination)(state)
  return simulateGuardPatrol(obstacles)(updatedState)
}

private def nextTile(currentPos: (Int, Int), heading: Heading): (Int, Int) = {
  heading match {
    case "n" => (currentPos._1 - 1, currentPos._2)
    case "e" => (currentPos._1, currentPos._2 + 1)
    case "s" => (currentPos._1 + 1, currentPos._2)
    case "w" => (currentPos._1, currentPos._2 - 1)
  }
}

private def isOutOfBounds(mapSize: (Int, Int))(pos: (Int, Int)): Boolean = {
  val (x, y) = pos
  (x < 0 || y < 0 || x >= mapSize._1 || y >= mapSize._2)
}

private def rotate(heading: Heading): Heading = {
  heading match {
    case "n" => "e"
    case "e" => "s"
    case "s" => "w"
    case "w" => "n"
  }
}

private def visit(
    tile: (Int, Int)
)(state: GuardPatrolState): GuardPatrolState = {
  val alreadyVisited = state.visited(tile._1)(tile._2)
  if (alreadyVisited) {
    state.copy(position = tile)
  } else {
    val updatedRow = state.visited(tile._1).updated(tile._2, true)
    val updatedVisited = state.visited.updated(tile._1, updatedRow)
    state.copy(
      visited = updatedVisited,
      numberVisited = state.numberVisited + 1,
      position = tile
    )
  }
}

private def createGuardPatrolState[T](
    obstacles: Array[Array[T]],
    position: (Int, Int),
    heading: Heading
): GuardPatrolState = {
  GuardPatrolState(
    visited = Range(0, obstacles.length)
      .map(i =>
        Range(0, obstacles(0).length)
          .map(j => (i, j) == position)
          .toArray
      )
      .toArray,
    numberVisited = 1,
    position = position,
    heading = heading
  )
}

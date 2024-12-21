package day14

val WIDTH = 101
val HEIGHT = 103

// val WIDTH = 11
// val HEIGHT = 7
def day14a(input: String): Long = {
  var robots = parseInput(input)
  // println(robots.mkString("\n"))
  // prettyPrint(robots)

  for (i <- Range(0, 100)) {
    robots = robots.map(_.advance())
  }

  computeScore(robots)
}

def day14b(input: String): Long = {
  var robots = parseInput(input)
  // println(robots.mkString("\n"))
  // prettyPrint(robots)

  for (i <- Range(0, 10000)) {
    robots = robots.map(_.advance())
    println(s"===== ${i + 1} seconds elapsed =====")
    prettyPrint(robots)
  }

  computeScore(robots)
}

private def parseInput(input: String): Seq[Robot] = {
  input
    .split("\n")
    .map(line => {
      val parsed =
        "p=(\\d+),(\\d+) v=(-?\\d+),(-?\\d+)".r.findFirstMatchIn(line).get
      Robot(
        (parsed.group(2).toInt, parsed.group(1).toInt),
        (parsed.group(4).toInt, parsed.group(3).toInt)
      )
    })
}

private case class Robot(position: (Int, Int), velocity: (Int, Int)) {
  def advance(): Robot = {
    val i = position._1 + velocity._1
    val j = position._2 + velocity._2

    copy(((i + HEIGHT) % HEIGHT, (j + WIDTH) % WIDTH))
  }
}

private def computeScore(robots: Seq[Robot]): Long = {
  quadrants
    .map(inQuadrant(robots))
    .map(_.length)
    .foldLeft(1)(_ * _)

}

private val quadrants: Seq[((Int, Int), (Int, Int))] =
  Seq(
    ((0, 0), (HEIGHT / 2, WIDTH / 2)),
    ((HEIGHT / 2 + 1, 0), (HEIGHT, WIDTH / 2)),
    ((0, WIDTH / 2 + 1), (HEIGHT / 2, WIDTH)),
    ((HEIGHT / 2 + 1, WIDTH / 2 + 1), (HEIGHT, WIDTH))
  )

private def inQuadrant(robots: Seq[Robot])(
    quadrant: ((Int, Int), (Int, Int))
): Seq[Robot] = {
  val (topLeft, bottomLeft) = quadrant

  robots.filter(r =>
    (topLeft._1 <= r.position._1) && (r.position._1 < bottomLeft._1) &&
      (topLeft._2 <= r.position._2) && (r.position._2 < bottomLeft._2)
  )
}

private def prettyPrint(robots: Seq[Robot]): Unit = {
  val map = Array.fill(HEIGHT)(Array.fill(WIDTH)("."))
  for (robot <- robots) {
    val position = map(robot.position._1)(robot.position._2)
    if (position == ".") {
      map(robot.position._1).update(robot.position._2, "1")
    } else {
      map(robot.position._1)
        .update(robot.position._2, (position.toInt + 1).toString())
    }

  }
  println()
  println(map.map(_.mkString("")).mkString("\n"))
  println()
}

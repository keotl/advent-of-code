package day11

import scala.annotation.tailrec

def day11a(input: String): Long = {
  val stones = input.strip().split(' ')

  stones
    .map(memoSimulateSingleRecursive(_, 25))
    .sum
}

def day11b(input: String): Long = {
  val stones = input.strip().split(' ')

  stones
    .map(memoSimulateSingleRecursive(_, 75))
    .sum
}

private def simulateSingleRecursive(
    stone: String,
    depth: Int
): Long = {
  if (depth == 0) {
    return 1
  }
  stone match {
    case "0" => memoSimulateSingleRecursive("1", depth - 1)
    case x if x.length % 2 == 0 => {
      val (a, b) = x.splitAt(x.length / 2)
      memoSimulateSingleRecursive(
        a.toLong.toString(),
        depth - 1
      ) + memoSimulateSingleRecursive(b.toLong.toString(), depth - 1)
    }
    case x =>
      memoSimulateSingleRecursive(
        (x.toLong * 2024).toString(),
        depth - 1
      )
  }
}

private def memoSimulateSingleRecursive(
    stone: String,
    depth: Int
): Long = {
  val cached = cache.get((stone, depth))
  if (cached.nonEmpty) {
    return cached.get
  }

  val result = simulateSingleRecursive(stone, depth)
  cache = cache + ((stone, depth) -> result)
  if (depth == 75) {
    println(s"depth=${depth}, cache size: ${cache.size}")
  }
  result
}

private var cache = Map[(String, Int), Long]()

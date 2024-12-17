package day7

def day7a(input: String): Long = {
  val parsed = parseInput(input)

  parsed
    .filter((target, operands) =>
      isValidCombination(target, operands.reverse, false)
    )
    .map(_._1)
    .sum
}

def day7b(input: String): Long = {
  val parsed = parseInput(input)

  parsed
    .filter((target, operands) =>
      isValidCombination(target, operands.reverse, true)
    )
    .map(_._1)
    .sum
}

private def parseInput(input: String): Seq[(Long, List[Long])] = {
  input
    .split("\n")
    .map(line => {
      val result :: operands :: _ = line.split(": ").toList: @unchecked
      (result.toLong, operands.split(" ").map(_.toLong).toList)
    })
}

private def isValidCombination(
    target: Long,
    operandsReversed: List[Long],
    enableConcatenation: Boolean
): Boolean = {
  // println(s"${target} : ${operandsReversed}")
  operandsReversed match {
    case Nil         => false
    case head :: Nil => target == head
    case head :: tail =>
      (target - head >= 0 && isValidCombination(
        target - head,
        tail,
        enableConcatenation
      )) || (target % head == 0 && isValidCombination(
        target / head,
        tail,
        enableConcatenation
      )) || (enableConcatenation && target
        .toString()
        .length > head.toString().length && target
        .toString()
        .endsWith(head.toString()) && isValidCombination(
        target
          .toString()
          .slice(0, target.toString().length - head.toString().length)
          .toLong,
        tail,
        enableConcatenation
      ))
  }
}

package day13

import scala.collection.mutable
import scala.collection.mutable.Queue
import scala.collection.mutable.PriorityQueue

def day13a(input: String): Long = {
  val machines = input.split("\n\n").map(parseMachine)

  machines
    .map(computeCosts)
    .filter(_.nonEmpty)
    .map(_.get)
    .sum
    .toLong
}

def day13b(input: String): Long = {
  val machines = input.split("\n\n").map(parseMachine)

  machines
    .map(m =>
      m.copy(target =
        (
          m.target._1 + 10000000000000L,
          m.target._2 + 10000000000000L
        )
      )
    )
    .map(computeCosts)
    .filter(_.nonEmpty)
    .map(_.get)
    .sum
    .toLong
}

private def parseMachine(input: String): MachineDef = {
  val rows = input.split("\n")
  val aMatch = buttonPattern.findFirstMatchIn(rows(0)).get
  val bMatch = buttonPattern.findFirstMatchIn(rows(1)).get
  val targetMatch = prizePattern.findFirstMatchIn(rows(2)).get
  MachineDef(
    (aMatch.group(1).toInt, aMatch.group(2).toInt),
    (bMatch.group(1).toInt, bMatch.group(2).toInt),
    (targetMatch.group(1).toInt, targetMatch.group(2).toInt)
  )
}

private val buttonPattern = "X\\+(\\d+), Y\\+(\\d+)".r
private val prizePattern = "X=(\\d+), Y=(\\d+)".r

case class MachineDef(
    a: (BigDecimal, BigDecimal),
    b: (BigDecimal, BigDecimal),
    target: (BigDecimal, BigDecimal)
)

private def computeCosts(machine: MachineDef): Option[BigDecimal] = {
  val bPresses =
    ((machine.target._2 - (machine.target._1 * machine.a._2) / machine.a._1)
      / (machine.b._2 - (machine.a._2 * machine.b._1) / machine.a._1))
  val aPresses =
    ((machine.target._1 - (machine.b._1 * bPresses)) / machine.a._1)

  val aRounded = Math.round(aPresses.toDouble)
  val bRounded = Math.round(bPresses.toDouble)

  val actualPos = (
    aRounded * machine.a._1 + bRounded * machine.b._1,
    aRounded * machine.a._2 + bRounded * machine.b._2
  )

  val distance =
    ((actualPos._1 - machine.target._1).abs + (actualPos._2 - machine.target._2).abs)

  if (distance < 0.001 && aPresses >= 0 && bPresses >= 0) {
    Some(aPresses * 3 + bPresses)
  } else {
    None
  }
}

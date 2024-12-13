package day3
import utils._
import cats.data.State
import scala.util.matching.Regex.Match
import cats.syntax.applicative._ // for pure

def day3a(input: String): Int = {
  val matches = PATTERN.findAllMatchIn(input)
  matches
    .map(m => {
      m.group(1).toInt * m.group(2).toInt
    })
    .sum()
}

val PATTERN = "mul\\((\\d+),(\\d+)\\)".r

def day3b(input: String): Int = {
  val matches = "(mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\))".r
    .findAllMatchIn(input)

  evalAll(matches.toList)
    .runA(true)
    .value

}

private def enableMult(acc: Int): CalcState[Int] = State(multEnabled => {
  (true, acc)
})

private def disableMult(acc: Int): CalcState[Int] = State(multEnabled => {
  (false, acc)
})

private def doMult(a: Int, b: Int, acc: Int): State[Boolean, Int] =
  State(multEnabled => {
    (multEnabled, if (multEnabled) then a * b + acc else acc)
  })
type CalcState[A] = State[Boolean, A]

private def evalAll(tokens: List[Match]): CalcState[Int] = {
  tokens.foldLeft(0.pure[CalcState])((a, m) => {
    m match {
      case t if t.group(0).contains("do()") => a.flatMap(acc => enableMult(acc))
      case t if t.group(0).contains("don't()") =>
        a.flatMap(acc => disableMult(acc))
      case t if t.group(0).contains("mul(") =>
        a.flatMap(acc =>
          doMult(
            t.group(2).toInt,
            t.group(3).toInt,
            acc
          )
        )
    }
  })
}

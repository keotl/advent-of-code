package day2

import scala.annotation.tailrec

def day2a(input: String): Int = {
  val rows = input.split("\n")

  rows
    .map(_.split(" ").map(_.toInt).toList)
    .count(
      and(
        monotonicCriterion,
        changeAmplitudeCriterion
      )
    )
}

def day2b(input: String): Int = {
  val rows = input.split("\n")

  rows
    .map(_.split(" ").map(_.toInt).toList)
    .count(
      withOneRemoved(
        and(
          monotonicCriterion,
          changeAmplitudeCriterion
        )
      )
    )
}

private def monotonicCriterion(levels: List[Int]): Boolean = {
  val a = levels.head
  val b = levels.tail.head

  val direction = b - a
  monotonicRecurse(direction, levels.tail)
}

@tailrec private def monotonicRecurse(
    direction: Int,
    levels: List[Int]
): Boolean = {
  levels match {
    case Nil      => true
    case h :: Nil => true
    case a :: b :: tail => {
      if ((b - a) * direction <= 0) {
        false
      } else {
        monotonicRecurse(direction, b :: tail)
      }
    }
  }
}

@tailrec private def changeAmplitudeCriterion(levels: List[Int]): Boolean = {
  levels.match {
    case Nil      => true
    case h :: Nil => true
    case a :: b :: tail => {
      val changeAmplitude = Math.abs(b - a)
      if (changeAmplitude < 1 || changeAmplitude > 3) {
        false
      } else {
        changeAmplitudeCriterion(b :: tail)
      }
    }
  }
}

private def and[T](predicates: (T => Boolean)*): (T) => Boolean =
  (t: T) => predicates.forall(_(t))

private def withOneRemoved(
    predicate: List[Int] => Boolean
): (levels: List[Int]) => Boolean = {
  // This runs on O(n²) by generating all valid subsequences with one
  // (or 0) items removed. It should be possible to use a greedy
  // algorithm to improve to O(n) runtime.
  // 1. Iterate through levels, keeping track of whether the "error
  // correcting allowance" has been spent or not.
  //
  // 2. If the next value is not allowed given the previous value,
  // decrease the EC allowance by one and continue.
  //
  // 3. We will need to iterate through levels twice, since the
  // monotonic criterion can only be checked greedily if we *know* the
  // expected direction.
  //
  // The greedy approach can only work if all criteria can be checked
  // greedily. Therefore the O(n²) approach gives us the flexibility
  // to add other types of criteria, such as "all numbers have to add
  // up to an even number". The choice would have to be a tradeoff
  // between input size and criterion-design constraints.
  def result(levels: List[Int]): Boolean = {
    generateValidSequences(levels, 1)
      .find(predicate)
      .nonEmpty
  }

  return result
}

private def generateValidSequences(
    values: List[Int],
    allowedRemovals: Int
): List[List[Int]] = {

  if (allowedRemovals == 0) {
    return List(values)
  }
  values match {
    case Nil      => Nil
    case h :: Nil => List(List(h), Nil)
    case h :: tail => {
      // TODO - This would have to be made tail-recursive to not
      // exceed stack depth - keotl 2024-12-13

      // either remove h
      generateValidSequences(tail, 0) ++
        // or remove later in tail
        generateValidSequences(tail, 1).map(h :: _)

    }
  }
}

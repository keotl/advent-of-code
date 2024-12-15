package day5

import scala.annotation.tailrec

def day5a(input: String): Int = {
  val (rules :: updates :: _) = (input.split("\n\n").toList: @unchecked)
  val dependencies = buildAncestorsMap(rules.split("\n"))

  updates
    .split("\n")
    .map(_.split(",").map(_.toInt))
    .filter(validateUpdateOrder(dependencies))
    .map(pages => pages(pages.length / 2))
    .sum()
}

def day5b(input: String): Int = {
  val (rules :: updates :: _) = (input.split("\n\n").toList: @unchecked)
  val dependencies = buildAncestorsMap(rules.split("\n"))

  updates
    .split("\n")
    .map(_.split(",").map(_.toInt).toSeq)
    .filter(x => !validateUpdateOrder(dependencies)(x))
    .map(reorderPages(dependencies)(_, Seq.empty))
    .map(pages => pages(pages.length / 2))
    .sum()
}

private def buildAncestorsMap(rules: Seq[String]): Map[Int, Set[Int]] = {
  rules
    .map(
      _.split('|')
        .map(_.toInt)
    )
    .groupBy(_(1))
    .view
    .mapValues(_.map(_(0)).toSet)
    .toMap
}

private def validateUpdateOrder(
    dependencies: Map[Int, Set[Int]]
)(updateOrder: Seq[Int]): Boolean = {

  try {
    updateOrder.foldRight(Set[Int]())((e, seen) => {
      if (seen.intersect(dependencies.getOrElse(e, Set.empty)).size > 0) {
        throw new Error("Dependency after")
      }
      seen + e
    })
    true
  } catch {
    case e: Error => {
      // println(e)
      false
    }
  }
}

@tailrec private def reorderPages(
    dependencies: Map[Int, Set[Int]]
)(incorrectOrder: Seq[Int], acc: Seq[Int]): Seq[Int] = {
  if (incorrectOrder.isEmpty) {
    return acc
  }
  val relevantDependencies = dependencies.view
    .mapValues(deps => deps.filter(incorrectOrder.contains(_)))
    .toMap
  val next =
    incorrectOrder
      .find(relevantDependencies.getOrElse(_, Seq.empty).isEmpty)
      .get

  reorderPages(dependencies)(
    incorrectOrder.filter(_ != next),
    next +: acc
  )
}

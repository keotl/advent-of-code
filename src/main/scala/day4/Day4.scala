package day4

import scala.util.matching.Regex

def day4a(input: String): Int = {
  val rowLength = input.split("\n").head.length()
  val searchText = input.replace("\n", "")
  buildRegexPatterns("XMAS", rowLength)
    .map(_.findAllMatchIn(searchText).length)
    .sum()
}

def buildRegexPatterns(searchPattern: String, rowLength: Int): Seq[Regex] = {
  Seq(
    // Forward
    searchPattern.r, // Horizontal
    buildPattern(searchPattern, rowLength - 2).r, // Diagonal left
    buildPattern(searchPattern, rowLength).r, // Diagonal right
    buildPattern(searchPattern, rowLength - 1).r, // Vertical

    // Backwards
    searchPattern.reverse.r, // Horizontal,
    buildPattern(searchPattern.reverse, rowLength - 2).r, // diagonal left
    buildPattern(searchPattern.reverse, rowLength).r, // diagonal right
    buildPattern(searchPattern.reverse, rowLength - 1).r // vertical
  )
}

private def findWithSpacing(start: Int, pattern: String, spacing: Int)(text: String) : Boolean = {

  for ((letter, index) <- pattern.zipWithIndex) {
    
  }
  false
}

def buildPattern(
    pattern: String,
    spacing: Int,
    placeholder: String = "."
): String =
  val separator = s"${placeholder}{${spacing}}"
  (pattern.head.toString) ++ "(?=" ++ separator ++ pattern.tail.toList.mkString(
    separator
  ) ++ ")"

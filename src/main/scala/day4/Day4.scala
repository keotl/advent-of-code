package day4

import scala.util.matching.Regex

def day4a(input: String): Int = {
  val rowLength = input.split("\n").head.length() + 1

  val patterns = buildSearchPatterns("XMAS", rowLength) ++ buildSearchPatterns(
    "SAMX",
    rowLength
  )
  input.zipWithIndex
    .map((_, startOffset) => {
      patterns.filter(_(startOffset)(input)).length
    })
    .sum
}

def buildSearchPatterns(searchPattern: String, rowLength: Int) = {
  Seq(
    findWithSpacing(searchPattern, 1), // Horizontal
    findWithSpacing(searchPattern, rowLength - 1), // Diagonal left
    findWithSpacing(searchPattern, rowLength), // Vertical
    findWithSpacing(searchPattern, rowLength + 1) // Diagonal right
  )
}

private def findWithSpacing(pattern: String, spacing: Int)(start: Int)(
    text: String
): Boolean = {
  // print(text)
  pattern.zipWithIndex.forall((letter, index) => {
    val searchPos = index * spacing + start
    searchPos < text.length && text.charAt(searchPos) == letter
  })
}

def day4b(input: String): Int = {
  val rows = input.split("\n")
  var result = 0
  for (i <- Range(0, rows.length - 2)) {
    for (j <- Range(0, rows.length - 2)) {
      if (checkMainDiagonal(i, j)(rows) && checkOppositeDiagonal(i, j)(rows)) {
        result += 1
      }
    }
  }

  result
}

def checkMainDiagonal(i: Int, j: Int)(rows: Array[String]): Boolean = {
  (rows(i).charAt(j) == 'M' && rows(i + 1).charAt(j + 1) == 'A' && rows(i + 2)
    .charAt(
      j + 2
    ) == 'S') || (rows(i)
    .charAt(j) == 'S' && rows(i + 1).charAt(j + 1) == 'A' && rows(i + 2).charAt(
    j + 2
  ) == 'M')
}

def checkOppositeDiagonal(i: Int, j: Int)(rows: Array[String]): Boolean = {
  (rows(i).charAt(j + 2) == 'M' && rows(i + 1).charAt(j + 1) == 'A' && rows(
    i + 2
  ).charAt(j) == 'S') || (rows(i).charAt(j + 2) == 'S' && rows(i + 1).charAt(
    j + 1
  ) == 'A' && rows(i + 2).charAt(j) == 'M')
}

package day4

import munit.FunSuite

class Day4Spec extends FunSuite {

  test("diagonal left") {
    val result = day4a("""...X
..M.
.A..
S...""")
    assertEquals(result, 1)
  }

  test("diagonal right") {
    val result = day4a("""X...
.M..
..A.
...S""")
    assertEquals(result, 1)
  }

  test("vertical + combined diagonals") {
    val result = day4a("""XXXX
MMMM
AAAA
SSSS""")
    assertEquals(result, 6)
  }
  test("reverse vertical") {
    val result = day4a("""S...
A...
M...
X...""")
    assertEquals(result, 1)
  }

  test("horizontal") {
    val result = day4a("""XMAS
SAMX""")
    assertEquals(result, 2)
  }
}

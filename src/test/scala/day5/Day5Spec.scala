package day5

import munit.FunSuite

class Day5Spec extends FunSuite {
  test("simplest case") {
    val result = day5a("""1|2

1,2,3""")

    assertEquals(result, 2)
  }

    test("5b simple case") {
    val result = day5b("""1|2
1|3

2,3,1""")

    assertEquals(result, 2)
  }
}

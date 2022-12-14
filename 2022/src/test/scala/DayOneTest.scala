package works.worace.aoc

trait TestUtil {
  def resourceLines(resourceName: String): Iterator[String] = {
    scala.io.Source
      .fromInputStream(getClass.getClassLoader.getResourceAsStream(resourceName))
      .getLines
  }
}

class DayOne extends munit.FunSuite with TestUtil {
  // part one
  def mostCalsForTopNElves(n: Int = 3): List[Int] = {
    var group = List[Int]()
    var topN: List[Int] = List()

    (resourceLines("dayone.txt") ++ Iterator("")).foreach { line =>
      if (line.isEmpty) {
        val total = group.sum
        topN = (total :: topN).sortBy(-_).take(n)
        group = List[Int]()
      } else {
        group = line.toInt :: group
      }
    }
    topN
  }

  test("Day One Part 1") {
    assertEquals(mostCalsForTopNElves(1).head, 69289)
  }

  test("Day One Part 2") {
    assertEquals(mostCalsForTopNElves(3).sum, 205615)
  }
}

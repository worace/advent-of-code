package works.worace.aoc

trait Util {
  def resourceLines(resourceName: String): Iterator[String] = {
    scala.io.Source
      .fromInputStream(getClass.getClassLoader.getResourceAsStream(resourceName))
      .getLines
  }
}
class Aoc extends munit.FunSuite with Util

class DayOne extends Aoc {
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

class DayTwo extends Aoc {
  sealed trait Move {
    def moveScore: Int
    def beats: Move
    def loses: Move
    def vsScore(opp: Move): Int = {
      val roundScore = if (this == opp) {
        3
      } else if (this.beats == opp) {
        6
      } else {
        0
      }
      roundScore + moveScore
    }
    def toGetResultAgainst(result: Result): Move = {
      result match {
        case Win => loses
        case Lose => beats
        case Draw => this
      }
    }
  }

  case object Rock extends Move {
    def moveScore: Int = 1
    def beats: Move = Scissors
    def loses: Move = Paper
  }
  case object Paper extends Move {
    def moveScore: Int = 2
    def beats: Move = Rock
    def loses: Move = Scissors
  }
  case object Scissors extends Move {
    def moveScore: Int = 3
    def beats: Move = Paper
    def loses: Move = Rock
  }

  object Move {
    val opps = Map("A" -> Rock, "B" -> Paper, "C" -> Scissors)
    def opp(s: String): Move = opps(s)
    val yous = Map("X" -> Rock, "Y" -> Paper, "Z" -> Scissors)
    def you(s: String): Move = yous(s)
  }

  sealed trait Result
  case object Win extends Result
  case object Lose extends Result
  case object Draw extends Result
  object Result {
    val froms = Map("X" -> Lose, "Y" -> Draw, "Z" -> Win)
    def from(s: String): Result = froms(s)
  }

  def movePair(opp: String, result: String): (Move, Move) = {
    val o = Move.opp(opp)
    println(s"opp move: ${o}")
    println(s"res move: ${Result.from(result)}")
    println(Paper.toGetResultAgainst(Lose))
    (o, o.toGetResultAgainst(Result.from(result)))
  }

  test("Day Two Part 1") {
    assertEquals(Move.opp("A"), Rock)
    assertEquals(Move.you("Z"), Scissors)
    assertEquals(Rock.vsScore(Scissors), 7)
    assertEquals(Rock.vsScore(Paper), 1)
    assertEquals(Paper.vsScore(Rock), 8)
    assertEquals(Paper.vsScore(Paper), 5)
    assertEquals(Scissors.vsScore(Paper), 9)

    val total = resourceLines("daytwo.txt").map { line =>
      val Array(opp, you) = line.split(" ")
      Move.you(you).vsScore(Move.opp(opp))
    }.sum

    assertEquals(total, 13526)
  }

  test("Day Two Part 2") {
    assertEquals(Rock.toGetResultAgainst(Draw), Rock)
    assertEquals(Rock.toGetResultAgainst(Win), Paper)
    assertEquals(Rock.toGetResultAgainst(Lose), Scissors)
    assertEquals(Paper.toGetResultAgainst(Lose), Rock)
    assertEquals(Result.from("X"), Lose)
    assertEquals(Result.from("Y"), Draw)
    assertEquals(Result.from("Z"), Win)
    assertEquals(movePair("A", "Y"), (Rock, Rock))
    assertEquals(movePair("B", "X"), (Paper, Rock))

    val total = resourceLines("daytwo.txt").map { line =>
      val Array(opp, res) = line.split(" ")
      val oppMove = Move.opp(opp)
      val goalResult = Result.from(res)
      val yourMove = oppMove.toGetResultAgainst(goalResult)

      yourMove.vsScore(oppMove)
    }.sum

    assertEquals(total, 14204)
  }
}

package asciipath

import asciipath.model._
import utest._

object AsciiMapTraverseTest extends TestSuite {
  val service      = new AsciiMapService(new AsciiMapValidator)
  val tests: Tests = Tests {
    test("Valid maps") {
      test("Basic map") {
        service.traverse(
          """
            |  @---A---+
            |          |
            |  x-B-+   C
            |      |   |
            |      +---+
            |""".stripMargin) ==> Right(AsciiMapPath("ACB", "@---A---+|C|+---+|+-B-x"))
      }
      test("Go straight through intersections") {
        service.traverse(
          """
            |  @
            |  | +-C--+
            |  A |    |
            |  +---B--+
            |    |      x
            |    |      |
            |    +---D--+
            |""".stripMargin) ==> Right(AsciiMapPath("ABCD", "@|A+---B--+|+--C-+|-||+---D--+|x"))
      }
      test("Letters may be found on turns") {
        service.traverse(
          """  @---A---+
            |          |
            |  x-B-+   |
            |      |   |
            |      +---C
            |""".stripMargin) ==> Right(AsciiMapPath("ACB", "@---A---+|||C---+|+-B-x"))
      }
      test("Do not collect letters twice") {
        service.traverse(
          """    +--B--+
            |    |   +-C-+
            | @--A-+ | | |
            |    | | +-+ D
            |    +-+     |
            |            x
            |""".stripMargin) ==> Right(AsciiMapPath("ABCD", "@--A-+|+-+|A|+--B--+C|+-+|+-C-+|D|x"))
      }
      test("keep direction, even in a compact space") {
        service.traverse(
          """ +-B-+
            | |  +C-+
            |@A+ ++ D
            | ++    x
            |""".stripMargin) ==> Right(AsciiMapPath("ABCD", "@A+++A|+-B-+C+++C-+Dx"))
      }
      test("KUKURIKU") {
        service.traverse(
          """  @---+
            |      K
            |K-----|--I
            ||     |  |
            ||  +--U  |
            ||  |     |
            |+--K--Ux R
            |   |     |
            |   +--U--+
            |""".stripMargin) ==> Right(AsciiMapPath("KUKURIKU", "@---+K||U--+|K|+--U--+|R|||I--|-----K|||+--K--Ux"))
      }
    }
    test("Invalid maps") {
      test("Empty map") {
        service.traverse(
          """""".stripMargin) ==> Left(EmptyAsciiMap())
      }
      test("No start") {
        service.traverse(
          """     -A---+
            |          |
            |  x-B-+   C
            |      |   |
            |      +---+
            |""".stripMargin) ==> Left(NoStart())
      }
      test("No end") {
        service.traverse(
          """   @--A---+
            |          |
            |    B-+   C
            |      |   |
            |      +---+
            |""".stripMargin) ==> Left(NoEnd())
      }
      test("Multiple starts") {
        service.traverse(
          """   @--A-@-+
            |          |
            |  x-B-+   C
            |      |   |
            |      +---+
            |""".stripMargin) ==> Left(MultipleStarts(Set(Cell(8, 0, '@'), Cell(3, 0, '@'))))
      }
      test("Multiple ends") {
        service.traverse(
          """  @--A----+
            |          |
            |  x-Bx+   C
            |      |   |
            |      +---+
            |""".stripMargin) ==> Left(MultipleEnds(Set(Cell(5, 2, 'x'), Cell(2, 2, 'x'))))
      }
      test("T forks") {
        service.traverse(
          """       x-B+
            |          |
            |   @--A---+
            |          |
            |     x+   C
            |      |   |
            |      +---+
            |""".stripMargin) ==> Left(TForks(Set(Cell(10, 2, '+'))))
      }
      test("Multiple roads from start") {
        service.traverse(
          """  @--A----+
            |  |       |
            |          |
            |  x-B-+   C
            |      |   |
            |      +---+
            |""".stripMargin) ==> Left(MultipleRoadsFromStart(Set(Cell(2, 0, '@'))))
      }
      test("Multiple roads from end") {
        service.traverse(
          """  @--A----+
            |          |
            |          |
            |  x-B-+   C
            |  |   |   |
            |  |   +---+
            |""".stripMargin) ==> Left(MultipleRoadsFromEnd(Set(Cell(2, 3, 'x'))))
      }
    }
  }
}
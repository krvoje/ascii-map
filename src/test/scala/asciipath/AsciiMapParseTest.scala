package asciipath
import model.AsciiMap
import asciipath.model._
import utest._

object AsciiMapParseTest extends TestSuite {
  val service      = new AsciiMapService(new AsciiMapValidator)
  val tests: Tests = Tests {
    test("Map is parsed correctly") {
      service.parse(
        """
          |  @---A---+
          |          |
          |  x-B-+   C
          |      |   |
          |      +---+
          |""".stripMargin) ==> AsciiMap(Set(
          Cell(2,1,'@'),Cell(3,1,'-'),Cell(4,1,'-'),Cell(5,1,'-'),Cell(6,1,'A'),Cell(7,1,'-'),Cell(8,1,'-'),Cell(9,1,'-'),Cell(10,1,'+'),
                                                                                                                          Cell(10,2,'|'),
          Cell(2,3,'x'),Cell(3,3,'-'),Cell(4,3,'B'),Cell(5,3,'-'),Cell(6,3,'+'),                                          Cell(10,3,'C'),
                                                                  Cell(6,4,'|'),                                          Cell(10,4,'|'),
                                                                  Cell(6,5,'+'),Cell(7,5,'-'),Cell(8,5,'-'),Cell(9,5,'-'),Cell(10,5,'+'),
      ))
    }
    test("Invalid characters are ignored") {
      service.parse("ABC DEF") ==> AsciiMap(Set(Cell(0,0,'A'), Cell(1,0,'B'), Cell(2,0,'C'), Cell(4,0,'D'),Cell(5,0,'E'),Cell(6,0,'F')))
    }
    test("Empty map") {
      service.parse("") ==> AsciiMap(Set.empty)
    }
  }
}
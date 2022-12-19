import org.scalatest.funsuite.AnyFunSuite

class ConverterTest extends AnyFunSuite {
  val starredLines = Seq(new StarredWordLine(3, "the first line"),
    new StarredWordLine(5, "the second line"))
  val wordLines = Seq(new WordLine("the third line"),
    new WordLine("the fourth line"))

  test("In projector StarredWordLine it's possible to project Page[StarredWordLine]") {
    assert(new Projector[StarredWordLine](StarConverter).project(
      new HelloPage[StarredWordLine](starredLines)) == "***the first line***\n*****the second line*****\n")
  }
  test("In projector StarredWordLine it's impossible to project Page[StarredWordLine]") {
    assertDoesNotCompile("new Projector[StarredWordLine](StarConverter).project" +
      "(new HelloPage[WordLine](wordLines))")
  }
  test("In projector WordLine it's possible to project Page[StarredWordLine]") {
    assert(new Projector[WordLine](LineConverter).project(
      new HelloPage[WordLine](starredLines)) == "the first line\nthe second line\n")
  }
  test("In projector WordLine it's possible to project Page[WordLine]") {
    assert(new Projector[WordLine](LineConverter).project(
      new HelloPage[WordLine](wordLines)) == "the third line\nthe fourth line\n")
  }
  test("In projector StarredWordLine it's possible to use Converter[WordLine]") {
    assert(new Projector[StarredWordLine](LineConverter).project(
      new HelloPage[StarredWordLine](starredLines)) == "the first line\nthe second line\n")
  }
  test("In projector StarredWordLine it's possible to use Converter[StarredWordLine]") {
    assert(new Projector[StarredWordLine](LineConverter).project(
      new HelloPage[StarredWordLine](starredLines)) == "the first line\nthe second line\n")
  }
  test("In projector Wordline it's possible to use Converter[WordLine]") {
    assert(new Projector[WordLine](LineConverter).project(
      new HelloPage[WordLine](wordLines)) == "the third line\nthe fourth line\n")
  }
  test("In projector Wordline it's impossible to use Converter[StarredWordLine]") {
    assertDoesNotCompile("new Projector[WordLine](StarConverter).project(new HelloPage[WordLine](wordLines))")
  }
}

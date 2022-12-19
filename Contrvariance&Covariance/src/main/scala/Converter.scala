trait Converter[-S] {
  def convert(value: S): String
}

trait Page[R] {
  def read: (Option[R], Page[R])
}

class Projector[R](converter: Converter[R]){
  def project(screen: Page[R]): String = screen.read match {
    case (Some(value), nextPage) => converter.convert(value) + project(nextPage)
    case (None, _) => ""
  }
}

class WordLine(val word: String)
class StarredWordLine(val stars: Int, word: String) extends WordLine(word)

object LineConverter extends Converter[WordLine] {
  override def convert(value: WordLine): String = value.word + '\n'
}

object StarConverter extends Converter[StarredWordLine] {
  override def convert(value: StarredWordLine): String = {
    val starString: String = "*" * value.stars
    starString + value.word + starString + '\n'
  }
}

class HelloPage[R <: WordLine](lines: Seq[R]) extends Page[R] {
  override def read: (Option[R], Page[R]) = lines match {
    case (head:R) ::(tail:Seq[R]) => (Some(head), new HelloPage(tail))
    case Nil => (None, this)
  }
}

import Content.{Table, Text}
import ParserSecondTask.getParse
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class ParserSecondTaskTest extends AsyncWordSpec with Matchers {

  "Parser" when {
    "gets content" should {
      "return content" in {
        val text = "text\n\ntable1\ttable1\n\nheader\ntable2\ttable2\nfooter\n\ntext"
        assert(getParse(text) == List(
          Text(List("text")),
          Table(Some(List()),List(List("table1", "table1")),Some(List())),
          Table(Some(List("header")),List(List("table2", "table2")),Some(List("footer"))),
          Text(List("text")))
        )
      }
    }
  }
}

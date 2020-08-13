import ParserFirstTask.table
import fastparse.{Parsed, parse}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class ParserFirstTaskTest extends AsyncWordSpec with Matchers {

  def getParse(text: String): Seq[Seq[String]] = parse(text, table(_)) match {
    case Parsed.Success(value, _) => value
  }

  "Parser" when {

    "gets content" should {
      "return content" in {
        val text = "one\ttwo\tthree\n1\t2\t3"
        assert(getParse(text) == List(List("one","two","three"),List("1","2","3")))
      }
    }

    "gets content with header" should {
      "return content" in {
        val text = "header\none\ttwo\tthree\n1\t2\t3"
        assert(getParse(text) == List(List("one","two","three"),List("1","2","3")))
      }
    }

    "gets content with footer" should {
      "return content" in {
        val text = "one\ttwo\tthree\n1\t2\t3\nfooter"
        assert(getParse(text) == List(List("one","two","three"),List("1","2","3")))
      }
    }
  }

  "gets content between header and footer" should {
    "return only content" in {
      val text = "header1\nheader2\none\ttwo\tthree\n1\t2\t3\nfooter1\nfooter2"
      assert(getParse(text) == List(List("one","two","three"),List("1","2","3")))
    }
  }
}

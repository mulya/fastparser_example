import Content.{Table, Text}
import ParserThirdTask.getParse
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class ParserThirdTaskTest extends AsyncWordSpec with Matchers {

  "Parser" when {
    "gets config text-table-table-text" should {
      "return list of Text,Table,Table,Text classes" in {
        val config = "text\ntable\ntable\ntext"
        val text = "text\ntext\n\ntable\ttable\n\nheader\ntable\ttable\nfooter\n\ntext\ntext"
        assert(getParse(config, text) == List(
          Text(List("text","text")),
          Table(Some(List()),List(List("table", "table")),Some(List())),
          Table(Some(List("header")),List(List("table", "table")),Some(List("footer"))),
          Text(List("text","text")))
        )
      }
    }

    "gets config table-text-text-table" should {
      "return list of Table,Text,Text,Table classes" in {
        val config = "table\ntext\ntext\ntable"
        val text = "table\ttable\n\ntext\ntext\n\ntext\ntext\n\nheader\ntable\ttable\nfooter"
        assert(getParse(config, text) == List(
            Table(Some(List()),List(List("table", "table")),Some(List())),
            Text(List("text","text")),
            Text(List("text","text")),
            Table(Some(List("header")),List(List("table", "table")),Some(List("footer")))
          )
        )
      }
    }
  }
}

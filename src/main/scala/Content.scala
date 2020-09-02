sealed trait Content

object Content {
  case class Table(header: Option[Seq[String]], table: Seq[Seq[String]], footer: Option[Seq[String]]) extends Content
  case class Text(list: Seq[String]) extends Content
}

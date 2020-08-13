sealed trait Content

object Content {
  case class Table(header: Option[Seq[String]] = None, table: Seq[Seq[String]], footer: Option[Seq[String]] = None) extends Content
  case class Text(list: Seq[String]) extends Content
}
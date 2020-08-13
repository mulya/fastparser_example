import Content.{Table, Text}
import Utils.{removeLineSeparator, removeLineSeparatorP}

import scala.io.Source

object ParserSecondTask extends App {
  import fastparse._
  import NoWhitespace._

  val file: String = Source.fromResource("file.txt").getLines().mkString("\n")

  def element[_: P] = P(CharIn("0-9", "A-Z", ".", "\\-", ":", " ", "(", ")", "%", "+", "#", "a-z", "/", "[", "]", "*").rep(1))
  def lineSeparator[_: P] = P("\n")

  def textLine[_: P] = removeLineSeparatorP(P(element ~ (End | lineSeparator)).!)
  def tableLine[_: P] = P((element ~ "\t").!.rep(1) ~ element ~ lineSeparator.?).!
  def splitTableLines[_: P] = P(tableLine.map(line => removeLineSeparator(line).split("\t").toList))

  def table[_: P] = P(textLine.rep.? ~ splitTableLines.rep ~ textLine.rep.?)
  def text[_: P] = P(textLine.rep)
  def fileParse[_: P] = P(Start ~ text ~ lineSeparator ~ table ~ lineSeparator ~ table ~ lineSeparator ~ text)

  def getParse(file: String): Seq[Content] = parse(file, fileParse(_)) match {
    case Parsed.Success(value, _) =>
      value.productIterator.map(content => content match {
        case (opt1: Option[Seq[String]], tab: Seq[Seq[String]], opt2: Option[Seq[String]]) => Table(opt1, tab, opt2)
        case seq: Seq[String] => Text(seq)
      }).toList
  }

  val result = getParse(file)
  println(result)
}
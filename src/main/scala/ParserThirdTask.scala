import Content.{ Table, Text }

import scala.io.Source

object ParserThirdTask extends App {
  import fastparse._
  import NoWhitespace._

  val file: String = Source.fromResource("file.txt").getLines().mkString("\n")
  val config: String = Source.fromResource("config.txt").getLines().mkString("\n")

  def element[_: P] = P(
    CharIn("0-9", "A-Z", ".", "\\-", ":", " ", "(", ")", "%", "+", "#", "a-z", "/", "[", "]", "*").rep(1)
  )
  def lineSeparator[_: P] = P("\n")

  def textLine[_: P] = P(element.! ~ lineSeparator)
  def textLineWithEnd[_: P] = P(element.! ~ lineSeparator.? ~ End.?)
  def header[_: P] = P(textLine.rep)
  def footer[_: P] = P(textLineWithEnd.rep)

  def tableLine[_: P] = P(element.!.rep(min = 2, sep = "\t") ~ lineSeparator.?)
  def readConfigLine[_: P] = P(element.! ~ (lineSeparator | End))

  def table[_: P]: P[Table] = P(lineSeparator.? ~ header.? ~ tableLine.rep ~ footer.?).map {
    case (header, table, footer) => Table(header, table, footer)
  }
  def text[_: P]: P[Text] = P(lineSeparator.? ~ textLineWithEnd.rep).map(content => Text(content))
  def configParse[_: P] = P(readConfigLine.rep)

  def getParse(config: String, file: String): Seq[Content] = parse(config, configParse(_)) match {
    case Parsed.Success(value, _) => {
      var index = 0
      value.map {
        case "table" => {
          parse(file, table(_), startIndex = index) match {
            case Parsed.Success(value, successIndex) => {
              index = successIndex
              value
            }
          }
        }
        case "text" => {
          parse(file, text(_), startIndex = index) match {
            case Parsed.Success(value, successIndex) => {
              index = successIndex
              value
            }
          }
        }
      }.toList
    }
  }

  val result = getParse(config, file)
  println(result)
}

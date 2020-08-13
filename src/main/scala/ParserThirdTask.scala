import Content.{Table, Text}
import Utils.{removeLineSeparator, removeLineSeparatorP}

import scala.io.Source

object ParserThirdTask extends App{
  import fastparse._
  import NoWhitespace._

  val file: String = Source.fromResource("file.txt").getLines().mkString("\n")
  val config: String = Source.fromResource("config.txt").getLines().mkString("\n")

  def element[_: P] = P(CharIn("0-9", "A-Z", ".", "\\-", ":", " ", "(", ")", "%", "+", "#", "a-z", "/", "[", "]", "*").rep(1))
  def lineSeparator[_: P] = P("\n")

  def textLine[_: P] = removeLineSeparatorP(P(element ~ lineSeparator).!)
  def textLineWithEnd[_: P] = removeLineSeparatorP(P(element ~ lineSeparator.? ~ End.?).!)
  def header[_: P] = P(textLine.rep)
  def footer[_: P] = P(textLineWithEnd.rep)
  def tableLine[_: P] = removeLineSeparatorP(P((element ~ "\t").!.rep(1) ~ element ~ lineSeparator.?).!)
  def splitTableLines[_: P] = P(tableLine.map(line => removeLineSeparator(line).split("\t").toList))
  def readConfigLine[_: P] = P(element ~ (lineSeparator | End)).!

  def table[_: P] = P(lineSeparator.? ~ header.? ~ splitTableLines.rep ~ footer.?)
  def text[_: P] = P(lineSeparator.? ~ textLineWithEnd.rep)
  def configParse[_: P] = P(readConfigLine.rep)

  def getParse(config: String, file: String): Seq[Content] = parse(config, configParse(_)) match {
    case Parsed.Success(value, _) => {
      var index = 0
      value.map(command => removeLineSeparator(command)).map {
        case "table" => {
          parse(file, table(_), startIndex = index) match {
            case Parsed.Success(value, successIndex) => {
              index = successIndex
              Table(value._1, value._2, value._3)
            }
          }
        }
        case "text" => {
          parse(file, text(_), startIndex = index) match {
            case Parsed.Success(value, successIndex) => {
              index = successIndex
              Text(value)
            }
          }
        }
      }.toList
    }
  }

  val result = getParse(config, file)
  println(result)
}

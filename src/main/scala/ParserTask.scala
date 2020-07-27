import scala.io.Source

object ParserTask extends App {
  import fastparse._, NoWhitespace._

  val lines = Source.fromResource("table.txt").getLines().mkString("\n")

  def element[_: P] = P(CharIn("0-9","A-Z",".","\\-",":"," ","(",")","%","+","#","a-z","/").rep(1))
  def endline[_: P] = P("\n")

  def garbageLine[_: P] = P(element ~ endline)
  def header[_: P] = P(Start ~ garbageLine.rep)
  def footer[_: P] = P(garbageLine.rep ~ element ~ End)

  def line[_: P] = P((element ~ "\t").!.rep(1) ~ element ~ endline.?).!
  def splitedLine[_: P] =
    P(line.map(x => x.replaceAll("(\r\n)|\r|\n", "")
    .split("\t")
    .toList))
  def table[_: P] =
    P(header.? ~ splitedLine.rep ~ footer.?)

  parse(lines, table(_)) match {
    case Parsed.Success(value, successIndex) =>
      println("Success value=" + value + " successIndex=" + successIndex)
    case f @ Parsed.Failure(_) =>
      println("Failure " + f.trace(true))
  }
}
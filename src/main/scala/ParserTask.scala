import scala.io.Source

object ParserTask extends App {
  import fastparse._, NoWhitespace._

  val lines = Source.fromResource("table.txt").getLines().mkString("\n")

  def elementRow[_: P] = P(CharIn("0-9","A-Z",".","\\-",":"," ","(",")","%","+","#","a-z","/").rep(1).!)
  def endline[_: P] = P("\n")
  def tableRow[_: P] = P((elementRow ~ "\t".?).rep ~ endline).map(list => if (list.length == 9) list)
  def table[_: P] = P(tableRow.rep(1))

  parse(lines, table(_)) match {
    case Parsed.Success(value, successIndex) =>
      println("Success value=" + value + " successIndex=" + successIndex)
    case f @ Parsed.Failure(label, index, extra) =>
      println("Failure " + f.trace(true))
  }
}

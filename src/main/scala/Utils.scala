import fastparse.P

object Utils {
  def removeLineSeparatorP(parserLines: P[String]): P[String] =
    parserLines.map(line => removeLineSeparator(line))

  def removeLineSeparator(line: String): String =
    line.replaceAll("(\r\n)|\r|\n", "")
}

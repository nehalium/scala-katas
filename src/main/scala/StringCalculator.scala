/*
 * String Calculator
 *
 * See: http://osherove.com/tdd-kata-1/
 */
class StringCalculator {
  val numbersPattern = "\\/\\/(.*?)\\n(.*?)".r
  val delimitersPattern = "\\[(.*?)\\]".r
  val defaultDelimiters = ",|\\n"

  def add(numbers: String): Int = {
    numbers match {
      case numbersPattern(resolvedDelimiters, resolvedNumbers) =>
        add(resolvedNumbers, resolvedDelimiters)
      case _ =>
        add(numbers, defaultDelimiters)
    }
  }

  def add(numbers: String, delimiters: String): Int = {
    numbers
      .split(getSplitPattern(delimiters))
      .map(
        x => if (x.equals(""))
          0
        else
          Integer.parseInt(x, 10)
      )
      .map(
        x => if (x < 0)
          throw new Exception("negatives not allowed")
        else
          x
      )
      .filter(_ <= 1000)
      .sum
  }

  def getSplitPattern(delimiters: String) = {
    escapeRegexChars(decomposeDelimiters(delimiters))
  }

  def decomposeDelimiters(delimiters: String) = {
    delimiters match {
      case delimitersPattern(delimiterList) =>
        delimitersPattern
          .findAllMatchIn(delimiters)
          .map(x => x.group(1))
          .mkString("(", "|", ")")
      case _ =>
        delimiters
    }
  }

  def escapeRegexChars(pattern: String) = {
    "[\\*\\#\\+\\?\\$]".r.replaceAllIn(pattern, m => s"\\\\${m.toString}")
  }
}

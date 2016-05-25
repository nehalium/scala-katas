class StringCalculator {
  val numbersPattern = "\\/\\/(.*?)\\n(.*?)".r
  val delimitersPattern = "\\[(.*?)\\]".r
  val defaultDelimiters = ",|\\n"

  def Add(numbers: String): Int = {
    numbers match {
      case numbersPattern(resolvedDelimiters, resolvedNumbers) =>
        Add(resolvedNumbers, resolvedDelimiters)
      case _ =>
        Add(numbers, defaultDelimiters)
    }
  }

  def Add(numbers: String, delimiters: String): Int = {
    numbers
      .split(convertToRegexPattern(delimiters))
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
      .filter(x => x <= 1000)
      .sum
  }

  def convertToRegexPattern(delimiters: String) = {
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
    pattern
      .replace("*", "\\*")
      .replace("#", "\\#")
  }
}

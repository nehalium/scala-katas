class StringCalculator {
  val delimiterPattern = "\\/\\/(.*?)\\n".r
  val defaultSplitPattern = ",|\\n"

  def Add(numbers: String): Int = {
    getNumberList(numbers)
      .split(getSplitPattern(numbers))
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

  def getNumberList(numbers: String) = {
    delimiterPattern.replaceAllIn(numbers, "")
  }

  def getSplitPattern(numbers: String) = {
    val matchData = delimiterPattern.findAllIn(numbers).matchData.toList
    if (matchData.nonEmpty)
      matchData.head.group(1).replace("][", "")
    else
      defaultSplitPattern
  }
}

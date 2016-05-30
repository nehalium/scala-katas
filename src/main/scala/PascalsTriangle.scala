/*
 * Pascal's Triangle
 */
class PascalsTriangle {
  def getValue(columns: Int, rows: Int): Int = {
    if (columns > rows)
      throw new Exception("improper inputs")
    else if (columns == 0 || columns == rows)
      1
    else
      getValue(columns - 1, rows - 1) + getValue(columns, rows - 1)
  }
}

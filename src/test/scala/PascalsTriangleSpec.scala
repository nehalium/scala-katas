/*
 * Pascal's Triangle Unit Tests
 *
 * 1
 * 1 1
 * 1 2 1
 * 1 3 3  1
 * 1 4 6  4  1
 * 1 5 10 10 5  1
 * 1 6 15 20 15 6  1
 * 1 7 21 35 35 21 7 1
 *
 */
class PascalsTriangleSpec extends org.specs2.mutable.Specification {
  val pascal = new PascalsTriangle

  "PascalsTriangle should" >> {
    "throw an exception for column 1, row 0" >> {
      pascal.getValue(1, 0)  must throwA(new Exception("improper inputs"))
    }
    "return 1 for column 0, row 0" >> {
      pascal.getValue(0, 0) must_== 1
    }
    "return 6 for column 2, row 4" >> {
      pascal.getValue(2, 4) must_== 6
    }
    "return 21 for column 5, row 7" >> {
      pascal.getValue(5, 7) must_== 21
    }
  }
}

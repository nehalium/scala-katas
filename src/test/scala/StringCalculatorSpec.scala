/*
 * String Calculator Unit Tests
 */
class StringCalculatorSpec extends org.specs2.mutable.Specification {
  val calc = new StringCalculator

  "StringCalculator should" >> {
    "return 0 for empty string" >> {
      calc.add("") must_=== 0
    }
    "return 0 for empty string with commas" >> {
      calc.add(",") must_=== 0
    }
    "return 0 for '0'" >> {
      calc.add("0") must_=== 0
    }
    "return 1 for '1'" >> {
      calc.add("1") must_=== 1
    }
    "return 3 for '1,2'" >> {
      calc.add("1,2") must_=== 3
    }
    "throw an exception for '-1,1'" >> {
      calc.add("-1,1") must throwA(new Exception("negatives not allowed"))
    }
    "return 6 for '1,2,3'" >> {
      calc.add("1,2,3") must_=== 6
    }
    "return 6 for '1\n2,3'" >> {
      calc.add("1\n2,3") must_=== 6
    }
    "return 2 for '2,1001'" >> {
      calc.add("2,1001") must_=== 2
    }
    "return 6 for '//;\n1;2;3'" >> {
      calc.add("//;\n1;2;3") must_=== 6
    }
    "return 6 for '//[***]\n1***2***3'" >> {
      calc.add("//[***]\n1***2***3") must_=== 6
    }
    "return 6 for '//[%][*]\n1*2%3'" >> {
      calc.add("//[%][*]\n1*2%3") must_=== 6
    }
    "return 6 for '//[%][*#*]\n1*#*2%3'" >> {
      calc.add("//[%][*#*]\n1*#*2%3") must_=== 6
    }
  }
}

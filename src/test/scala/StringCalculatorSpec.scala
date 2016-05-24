class StringCalculatorSpec extends org.specs2.mutable.Specification {
  val calc = new StringCalculator

  "StringCalculator should" >> {
    "return 0 for empty string" >> {
      calc.Add("") must_=== 0
    }
    "return 0 for empty string with commas" >> {
      calc.Add(",") must_=== 0
    }
    "return 0 for '0'" >> {
      calc.Add("0") must_=== 0
    }
    "return 1 for '1'" >> {
      calc.Add("1") must_=== 1
    }
    "return 3 for '1,2'" >> {
      calc.Add("1,2") must_=== 3
    }
    "return 0 for '-1,1'" >> {
      calc.Add("-1,1") must throwA(new Exception("negatives not allowed"))
    }
    "return 6 for '1,2,3'" >> {
      calc.Add("1,2,3") must_=== 6
    }
    "return 6 for '1\n2,3'" >> {
      calc.Add("1\n2,3") must_=== 6
    }
    "return 2 for '2,1001'" >> {
      calc.Add("2,1001") must_=== 2
    }
    "return 6 for '//;\n1;2;3'" >> {
      calc.Add("//;\n1;2;3") must_=== 6
    }
    "return 6 for '//[***]\n1***2***3'" >> {
      calc.Add("//[***]\n1***2***3") must_=== 6
    }
    "return 6 for '//[%][*]\n1*2%3'" >> {
      calc.Add("//[%][*]\n1*2%3") must_=== 6
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    TestSnippets.runTests()
    TestShonanDotProductOnly.runTests()
    TestShonanHMM.runTests()
    TestStymonas.runTests()
  }
}
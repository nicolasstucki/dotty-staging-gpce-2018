object Test {
  def main(args: Array[String]): Unit = {
    TestShonanDotProductOnly.runTests()
    TestShonanHMM.runTests()
    TestStagedStreams.runTests()
  }
}
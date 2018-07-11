import scala.quoted._

object TestStagedStreams {
  implicit val toolbox: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make

  def runTests(): Unit = {
   println("streams")
  }

  private[this] var i = 1
  def assertEqual[T](actual: T, expected: T): Unit = {
    if (actual == expected) {
      println(s"Staged Streams test $i ok")
      i += 1
    } else {
      println(s"Staged Streams test $i: failed")
      println(s"Expeted:")
      println(expected)
      println(s"Actual:")
      println(actual)
      System.exit(1)
    }
  }
}

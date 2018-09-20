import scala.quoted._

import snippets._

object TestSnippets {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  def runTests(): Unit = {
    assertEqual(
      Section2.unrolledExample1.show,
      """{
        |  scala.Predef.println(1)
        |  scala.Predef.println(2)
        |  ()
        |}""".stripMargin)

    assertEqual(
      Section2.some('(3), '[Int]).show,
      "scala.Some.apply[scala.Int](3)")

    assertEqual(
        Section3.sumCode.show,
        """((arr: scala.Array[scala.Int]) => {
          |  var sum: scala.Int = 0
          |  var i: scala.Int = 0
          |  while (i.<(arr.length)) {
          |    val element: scala.Int = arr.apply(i)
          |    sum = sum.+(element)
          |    i = i.+(1)
          |  }
          |  (sum: scala.Int)
          |})""".stripMargin)

    assertEqual(Section3.sum(Array(1, 2, 3)), 6)
    assertEqual(Section3.sum(Array(2, 3, 4, 5)), 14)


    assertEqual(
      ('{ (arr: Array[Int]) => ~Section4.sumNCode(3, '(arr)) }).show,
      """((arr: scala.Array[scala.Int]) => {
        |  { // inlined
        |    val assertion: scala.Boolean = arr.length.==(0.*(2).+(1).*(2).+(1))
        |    val DottyPredef$_this: dotty.DottyPredef = dotty.DottyPredef
        |    if (assertion.unary_!) DottyPredef$_this.assertFail() else ()
        |  }
        |  var sum: scala.Int = 0
        |  sum = sum.+(arr.apply(0))
        |  sum = sum.+(arr.apply(0.*(2).+(1)))
        |  sum = sum.+(arr.apply(0.*(2).+(1).*(2)))
        |  ()
        |  (sum: scala.Int)
        |})""".stripMargin)

    assertEqual(
      ('{ (arr: Array[Int]) => ~Section5.staged('(arr), '(x => println(~x))) }).show,
      """((arr: scala.Array[scala.Int]) => {
        |  var i: scala.Int = 0
        |  while (i.<(arr.length)) {
        |    val element: scala.Int = arr.apply(i)
        |    val x$1: scala.Int = element
        |    scala.Predef.println(x$1.unary_~)
        |    i = i.+(1)
        |  }
        |})""".stripMargin)
  }

  private[this] var i = 1
  def assertEqual[T](actual: T, expected: T): Unit = {
    if (actual == expected) {
      println(s"Snippets test $i ok")
      i += 1
    } else {
      println(s"Snippets test $i: failed")
      println(s"Expeted:")
      println(expected)
      println(s"Actual:")
      println(actual)
      System.exit(1)
    }
  }
}

import scala.quoted._

import strymonas._

object TestStymonas {
  implicit val toolbox: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make

  import strymonas.TestPipelines._

  def runTests(): Unit = {
    assertEqual(
      sum().show,
      """((array: scala.Array[scala.Int]) => {
        |  var x: scala.Int = 0
        |  var x: scala.Int = array.length
        |  var x: scala.Int = 0
        |  while (x.<(x)) {
        |    val el: scala.Int = array.apply(x)
        |    x = x.+(1)
        |    x = x.+(el)
        |  }
        |  x
        |})""".stripMargin)
    val t1 = sum().run
    assertEqual(t1(Array(1, 2, 3)), 6)
    assertEqual(t1(Array(1, 2, 3, 4)), 10)

    assertEqual(
      sumOfSquares().show,
      """((array: scala.Array[scala.Int]) => {
        |  var x: scala.Int = 0
        |  var x: scala.Int = array.length
        |  var x: scala.Int = 0
        |  while (x.<(x)) {
        |    val el: scala.Int = array.apply(x)
        |    x = x.+(1)
        |    x = x.+(el.*(el))
        |  }
        |  x
        |})""".stripMargin)
    val t2 = sumOfSquares().run
    assertEqual(t2(Array(1, 2, 3)), 14)
    assertEqual(t2(Array(1, 2, 3, 4)), 30)

    assertEqual(
      cart().show,
      """((vHi: scala.Array[scala.Int], vLo: scala.Array[scala.Int]) => {
        |  var x: scala.Int = 0
        |  var x: scala.Int = vHi.length
        |  var x: scala.Int = 0
        |  while (x.<(x)) {
        |    val el: scala.Int = vHi.apply(x)
        |    x = x.+(1)
        |    var x: scala.Int = vLo.length
        |    var x: scala.Int = 0
        |    while (x.<(x)) {
        |      val el: scala.Int = vLo.apply(x)
        |      x = x.+(1)
        |      x = x.+(el.*(el))
        |    }
        |  }
        |  x
        |})""".stripMargin)
    val t3 = cart().run
    assertEqual(t3(Array(1, 2, 3), Array(1, 2, 3)), 36)
    assertEqual(t3(Array(1, 2, 3, 4), Array(1, 2, 3, 4)), 100)

    assertEqual(
      filter().show,
      """((array: scala.Array[scala.Int]) => {
        |  var x: scala.Int = 0
        |  var x: scala.Int = array.length
        |  var x: scala.Int = 0
        |  while (x.<(x)) {
        |    val el: scala.Int = array.apply(x)
        |    x = x.+(1)
        |    if (el.%(2).==(0)) x = x.+(el) else ()
        |  }
        |  x
        |})""".stripMargin)
    val t4 = filter().run
    assertEqual(t4(Array(1, 2, 3)), 2)
    assertEqual(t4(Array(1, 2, 3, 4)), 6)

    assertEqual(
      take().show,
      """((array: scala.Array[scala.Int]) => {
        |  var x: scala.Int = 0
        |  var x: scala.Int = array.length
        |  var x: scala.Int = 0
        |  var x: scala.Int = 2
        |  while (x.>(0).&&(x.<(x))) {
        |    val el: scala.Int = array.apply(x)
        |    x = x.+(1)
        |    x = x.-(1)
        |    x = x.+(el)
        |  }
        |  x
        |})""".stripMargin)
    val t5 = take().run
    assertEqual(t5(Array(1, 2, 3)), 3)
    assertEqual(t5(Array(1, 2, 3, 4)), 3)

    assertEqual(
      flatMap_take().show,
      """((array1: scala.Array[scala.Int], array2: scala.Array[scala.Int]) => {
        |  var x: scala.Int = 0
        |  var x: scala.Int = array1.length
        |  var x: scala.Int = 0
        |  var x: scala.Int = 20000000
        |  while (x.>(0).&&(x.<(x))) {
        |    val el: scala.Int = array1.apply(x)
        |    x = x.+(1)
        |    var x: scala.Int = array2.length
        |    var x: scala.Int = 0
        |    while (x.>(0).&&(x.<(x))) {
        |      val el: scala.Int = array2.apply(x)
        |      x = x.+(1)
        |      x = x.-(1)
        |      x = x.+(el)
        |    }
        |  }
        |  x
        |})""".stripMargin)
    val t6 = flatMap_take().run
    assertEqual(t6(Array(1, 1, 1), Array(1, 2, 3)), 18)
    assertEqual(t6(Array(1, 1, 1, 1), Array(1, 2, 3, 4)), 40)

    assertEqual(
      dotProduct().show,
      """((array1: scala.Array[scala.Int], array2: scala.Array[scala.Int]) => {
        |  var x: scala.Int = 0
        |  var x: scala.Int = array1.length
        |  var x: scala.Int = 0
        |  var x: scala.Int = array2.length
        |  var x: scala.Int = 0
        |  while (x.<(x).&&(x.<(x))) {
        |    val el: scala.Int = array1.apply(x)
        |    x = x.+(1)
        |    val el: scala.Int = array2.apply(x)
        |    x = x.+(1)
        |    x = x.+(el.+(el))
        |  }
        |  x
        |})""".stripMargin)
    val t7 = dotProduct().run
    assertEqual(t7(Array(1, 2, 3), Array(1, 2, 3)), 12)
    assertEqual(t7(Array(1, 2, 3, 4), Array(1, 2, 3, 4)), 20)

    assertEqual(
      flatMap_after_zip().show,
      """((array1: scala.Array[scala.Int], array2: scala.Array[scala.Int]) => {
        |  var x: scala.Int = 0
        |  var x: scala.Int = array1.length
        |  var x: scala.Int = 0
        |  var x: scala.Int = array1.length
        |  var x: scala.Int = 0
        |  while (x.<(x).&&(x.<(x))) {
        |    val el: scala.Int = array1.apply(x)
        |    x = x.+(1)
        |    val el: scala.Int = array1.apply(x)
        |    x = x.+(1)
        |    var x: scala.Int = array2.length
        |    var x: scala.Int = 0
        |    while (x.<(x)) {
        |      val el: scala.Int = array2.apply(x)
        |      x = x.+(1)
        |      x = x.+(el.+(el).+(el))
        |    }
        |  }
        |  x
        |})""".stripMargin)
    val t8 = flatMap_after_zip().run
    assertEqual(t8(Array(1, 2, 3), Array(1, 2, 3) ), 54)
    assertEqual(t8(Array(1, 2, 3, 4), Array(1, 2, 3, 4)), 120)

    assertEqual(
      zip_after_flatMap().show,
      """((array1: scala.Array[scala.Int], array2: scala.Array[scala.Int]) => {
        |  var x: scala.Int = 0
        |  var x: scala.Int = array1.length
        |  var x: scala.Int = 0
        |  var x: scala.Int = array1.length
        |  var x: scala.Int = 0
        |  var x: scala.Boolean = x.<(x)
        |  while (x.&&(x.<(x))) {
        |    val el: scala.Int = array1.apply(x)
        |    x = x.+(1)
        |    var x: scala.Int = array2.length
        |    var x: scala.Int = 0
        |    while (x.&&(x.<(x))) {
        |      val el: scala.Int = array2.apply(x)
        |      x = x.+(1)
        |      val el: scala.Int = array1.apply(x)
        |      x = x.+(1)
        |      x = x.+(el.+(el).+(el))
        |      x = x.<(x)
        |    }
        |  }
        |  x
        |})""".stripMargin)
    val t9 = zip_after_flatMap().run
    assertEqual(t9(Array(1, 2, 3), Array(1, 2, 3)), 15)
    assertEqual(t9(Array(1, 2, 3, 4), Array(1, 2, 3, 4)), 24)

    assertEqual(
      zip_flat_flat().show,
      """((array1: scala.Array[scala.Int], array2: scala.Array[scala.Int]) => {
        |  var x: scala.Int = 0
        |  var x: scala.Int = array1.length
        |  var x: scala.Int = 0
        |  var x: scala.Function1[scala.Unit, scala.Unit] = ((_$8: scala.Unit) => ())
        |  var x: scala.Boolean = true
        |  var x: scala.Int = {
        |    null.asInstanceOf[scala.Int]
        |  }
        |  def adv: scala.Function1[scala.Unit, scala.Unit] = ((_$9: scala.Unit) => {
        |    x = x.<(x)
        |    if (x) {
        |      val el: scala.Int = array1.apply(x)
        |      x = x.+(1)
        |      var x: scala.Int = array2.length
        |      var x: scala.Int = 0
        |      val oldnadv: scala.Function1[scala.Unit, scala.Unit] = x
        |      val adv1: scala.Function1[scala.Unit, scala.Unit] = ((_$7: scala.Unit) => if (x.<(x)) {
        |        val el: scala.Int = array2.apply(x)
        |        x = x.+(1)
        |        x = el.+(el)
        |      } else {
        |        x = oldnadv
        |        oldnadv.apply(())
        |      })
        |      x = adv1
        |      adv1.apply(())
        |    } else ()
        |  })
        |  x = adv
        |  adv.apply(())
        |  var x: scala.Int = array2.length
        |  var x: scala.Int = 0
        |  var x: scala.Boolean = x
        |  var x: scala.Int = 20000000
        |  while (x.>(0).&&(x.&&(x.<(x)))) {
        |    val el: scala.Int = array2.apply(x)
        |    x = x.+(1)
        |    var x: scala.Int = array1.length
        |    var x: scala.Int = 0
        |    while (x.>(0).&&(x.&&(x.<(x)))) {
        |      val el: scala.Int = array1.apply(x)
        |      x = x.+(1)
        |      var el: scala.Int = x
        |      val f: scala.Function1[scala.Unit, scala.Unit] = x
        |      f.apply(())
        |      x = x.-(1)
        |      x = x.+(el.+(el.+(el)))
        |      x = x
        |    }
        |  }
        |  x
        |})""".stripMargin)
    val t10 = zip_flat_flat().run
    assertEqual(t10(Array(1, 2, 3), Array(1, 2, 3)), 72)
    assertEqual(t10(Array(1, 2, 3, 4), Array(1, 2, 3, 4)), 160)
  }

  private[this] var i = 1
  def assertEqual[T](actual: T, expected: T): Unit = {
    if (actual == expected) {
      println(s"Stymonas test $i ok")
      i += 1
    } else {
      println(s"Stymonas test $i: failed")
      println(s"Expected:")
      println(expected)
      println(s"Actual:")
      println(actual)
      System.exit(1)
    }
  }
}

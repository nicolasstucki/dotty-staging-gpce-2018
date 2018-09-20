import scala.quoted._

import strymonas._

object TestStymonas {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  import strymonas.TestPipelines._

  def runTests(): Unit = {
    assertEqual(
      sum().show,
      """((array: scala.Array[scala.Int]) => {
        |  var x: scala.Int = 0
        |  var x$2: scala.Int = array.length
        |  var x$3: scala.Int = 0
        |  while (x$3.<(x$2)) {
        |    val el: scala.Int = array.apply(x$3)
        |    x$3 = x$3.+(1)
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
        |  var x$2: scala.Int = array.length
        |  var x$3: scala.Int = 0
        |  while (x$3.<(x$2)) {
        |    val el: scala.Int = array.apply(x$3)
        |    x$3 = x$3.+(1)
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
        |  var x$2: scala.Int = vHi.length
        |  var x$3: scala.Int = 0
        |  while (x$3.<(x$2)) {
        |    val el: scala.Int = vHi.apply(x$3)
        |    x$3 = x$3.+(1)
        |    var x$4: scala.Int = vLo.length
        |    var x$5: scala.Int = 0
        |    while (x$5.<(x$4)) {
        |      val el$2: scala.Int = vLo.apply(x$5)
        |      x$5 = x$5.+(1)
        |      x = x.+(el.*(el$2))
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
        |  var x$2: scala.Int = array.length
        |  var x$3: scala.Int = 0
        |  while (x$3.<(x$2)) {
        |    val el: scala.Int = array.apply(x$3)
        |    x$3 = x$3.+(1)
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
        |  var x$2: scala.Int = array.length
        |  var x$3: scala.Int = 0
        |  var x$4: scala.Int = 2
        |  while (x$4.>(0).&&(x$3.<(x$2))) {
        |    val el: scala.Int = array.apply(x$3)
        |    x$3 = x$3.+(1)
        |    x$4 = x$4.-(1)
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
        |  var x$2: scala.Int = array1.length
        |  var x$3: scala.Int = 0
        |  var x$4: scala.Int = 20000000
        |  while (x$4.>(0).&&(x$3.<(x$2))) {
        |    val el: scala.Int = array1.apply(x$3)
        |    x$3 = x$3.+(1)
        |    var x$5: scala.Int = array2.length
        |    var x$6: scala.Int = 0
        |    while (x$4.>(0).&&(x$6.<(x$5))) {
        |      val el$2: scala.Int = array2.apply(x$6)
        |      x$6 = x$6.+(1)
        |      x$4 = x$4.-(1)
        |      x = x.+(el$2)
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
        |  var x$2: scala.Int = array1.length
        |  var x$3: scala.Int = 0
        |  var x$4: scala.Int = array2.length
        |  var x$5: scala.Int = 0
        |  while (x$3.<(x$2).&&(x$5.<(x$4))) {
        |    val el: scala.Int = array1.apply(x$3)
        |    x$3 = x$3.+(1)
        |    val el$2: scala.Int = array2.apply(x$5)
        |    x$5 = x$5.+(1)
        |    x = x.+(el.+(el$2))
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
        |  var x$2: scala.Int = array1.length
        |  var x$3: scala.Int = 0
        |  var x$4: scala.Int = array1.length
        |  var x$5: scala.Int = 0
        |  while (x$3.<(x$2).&&(x$5.<(x$4))) {
        |    val el: scala.Int = array1.apply(x$3)
        |    x$3 = x$3.+(1)
        |    val el$2: scala.Int = array1.apply(x$5)
        |    x$5 = x$5.+(1)
        |    var x$6: scala.Int = array2.length
        |    var x$7: scala.Int = 0
        |    while (x$7.<(x$6)) {
        |      val el$3: scala.Int = array2.apply(x$7)
        |      x$7 = x$7.+(1)
        |      x = x.+(el.+(el$2).+(el$3))
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
        |  var x$2: scala.Int = array1.length
        |  var x$3: scala.Int = 0
        |  var x$4: scala.Int = array1.length
        |  var x$5: scala.Int = 0
        |  var x$6: scala.Boolean = x$3.<(x$2)
        |  while (x$6.&&(x$5.<(x$4))) {
        |    val el: scala.Int = array1.apply(x$5)
        |    x$5 = x$5.+(1)
        |    var x$7: scala.Int = array2.length
        |    var x$8: scala.Int = 0
        |    while (x$6.&&(x$8.<(x$7))) {
        |      val el$2: scala.Int = array2.apply(x$8)
        |      x$8 = x$8.+(1)
        |      val el$3: scala.Int = array1.apply(x$3)
        |      x$3 = x$3.+(1)
        |      x = x.+(el.+(el$2).+(el$3))
        |      x$6 = x$3.<(x$2)
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
        |  var x$2: scala.Int = array1.length
        |  var x$3: scala.Int = 0
        |  var x$4: scala.Function1[scala.Unit, scala.Unit] = ((_$6: scala.Unit) => ())
        |  var x$5: scala.Boolean = true
        |  var x$6: scala.Int = {
        |    null.asInstanceOf[scala.Int]
        |  }
        |  def adv: scala.Function1[scala.Unit, scala.Unit] = ((_$7: scala.Unit) => {
        |    x$5 = x$3.<(x$2)
        |    if (x$5) {
        |      val el: scala.Int = array1.apply(x$3)
        |      x$3 = x$3.+(1)
        |      var x$7: scala.Int = array2.length
        |      var x$8: scala.Int = 0
        |      val oldnadv: scala.Function1[scala.Unit, scala.Unit] = x$4
        |      val adv1: scala.Function1[scala.Unit, scala.Unit] = ((_$5: scala.Unit) => if (x$8.<(x$7)) {
        |        val el$2: scala.Int = array2.apply(x$8)
        |        x$8 = x$8.+(1)
        |        x$6 = el.+(el$2)
        |      } else {
        |        x$4 = oldnadv
        |        oldnadv.apply(())
        |      })
        |      x$4 = adv1
        |      adv1.apply(())
        |    } else ()
        |  })
        |  x$4 = adv
        |  adv.apply(())
        |  var x$9: scala.Int = array2.length
        |  var x$10: scala.Int = 0
        |  var x$11: scala.Boolean = x$5
        |  var x$12: scala.Int = 20000000
        |  while (x$12.>(0).&&(x$11.&&(x$10.<(x$9)))) {
        |    val el$3: scala.Int = array2.apply(x$10)
        |    x$10 = x$10.+(1)
        |    var x$13: scala.Int = array1.length
        |    var x$14: scala.Int = 0
        |    while (x$12.>(0).&&(x$11.&&(x$14.<(x$13)))) {
        |      val el$4: scala.Int = array1.apply(x$14)
        |      x$14 = x$14.+(1)
        |      var el$5: scala.Int = x$6
        |      val f: scala.Function1[scala.Unit, scala.Unit] = x$4
        |      f.apply(())
        |      x$12 = x$12.-(1)
        |      x = x.+(el$5.+(el$3.+(el$4)))
        |      x$11 = x$5
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

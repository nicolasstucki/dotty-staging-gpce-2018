import scala.quoted._

import strymonas._

object TestStymonas {
  implicit val toolbox: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make

  def test1() = '{ (array: Array[Int]) =>
    ~Stream.of('(array))
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def test2() = '{ (array: Array[Int]) =>
    ~Stream.of('(array))
      .map((a: Expr[Int]) => '{ ~a * 2 })
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def test3() = '{ (array: Array[Int]) =>
    ~Stream.of('(array))
      .flatMap((d: Expr[Int]) => Stream.of('(array)).map((dp: Expr[Int]) => '{ ~d * ~dp }))
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def test4() = '{ (array: Array[Int]) =>
    ~Stream.of('(array))
      .filter((d: Expr[Int]) => '{ ~d % 2 == 0 })
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def test5() = '{ (array: Array[Int]) =>
    ~Stream.of('(array))
      .take('{2})
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def test6() = '{ (array1: Array[Int], array2: Array[Int]) =>
    ~Stream.of('(array1))
      .flatMap((d: Expr[Int]) => Stream.of('(array2)).take('{2}))
      .take('{5})
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def test7() = '{ (array: Array[Int]) =>
    ~Stream.of('(array))
      .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ ~a + ~b }), Stream.of('(array)))
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def test8() = '{ (array: Array[Int]) =>
    ~Stream.of('(array))
      .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ ~a + ~b }), Stream.of('(array)).flatMap((d: Expr[Int]) => Stream.of('(array)).map((dp: Expr[Int]) => '{ ~d + ~dp })))
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def test9() = '{ (array: Array[Int]) =>
    ~Stream.of('(array)).flatMap((d: Expr[Int]) => Stream.of('(array)).map((dp: Expr[Int]) => '{ ~d + ~dp }))
      .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ ~a + ~b }), Stream.of('(array)) )
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def test10() = '{ (array: Array[Int]) =>
    ~Stream.of('(array)).flatMap((d: Expr[Int]) => Stream.of('(array)).map((dp: Expr[Int]) => '{ ~d + ~dp }))
      .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ ~a + ~b }), Stream.of('(array)).flatMap((d: Expr[Int]) => Stream.of('(array)).map((dp: Expr[Int]) => '{ ~d + ~dp })) )
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def runTests(): Unit = {
    assertEqual(
      test1().show,
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
    val t1 = test1().run
    assertEqual(t1(Array(1, 2, 3)), 6)
    assertEqual(t1(Array(1, 2, 3, 4)), 10)

    assertEqual(
      test2().show,
      """((array: scala.Array[scala.Int]) => {
        |  var x: scala.Int = 0
        |  var x: scala.Int = array.length
        |  var x: scala.Int = 0
        |  while (x.<(x)) {
        |    val el: scala.Int = array.apply(x)
        |    x = x.+(1)
        |    x = x.+(el.*(2))
        |  }
        |  x
        |})""".stripMargin)
    val t2 = test2().run
    assertEqual(t2(Array(1, 2, 3)), 12)
    assertEqual(t2(Array(1, 2, 3, 4)), 20)

    assertEqual(
      test3().show,
      """((array: scala.Array[scala.Int]) => {
        |  var x: scala.Int = 0
        |  var x: scala.Int = array.length
        |  var x: scala.Int = 0
        |  while (x.<(x)) {
        |    val el: scala.Int = array.apply(x)
        |    x = x.+(1)
        |    var x: scala.Int = array.length
        |    var x: scala.Int = 0
        |    while (x.<(x)) {
        |      val el: scala.Int = array.apply(x)
        |      x = x.+(1)
        |      x = x.+(el.*(el))
        |    }
        |  }
        |  x
        |})""".stripMargin)
    val t3 = test3().run
    assertEqual(t3(Array(1, 2, 3)), 36)
    assertEqual(t3(Array(1, 2, 3, 4)), 100)

    assertEqual(
      test4().show,
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
    val t4 = test4().run
    assertEqual(t4(Array(1, 2, 3)), 2)
    assertEqual(t4(Array(1, 2, 3, 4)), 6)

    assertEqual(
      test5().show,
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
    val t5 = test5().run
    assertEqual(t5(Array(1, 2, 3)), 3)
    assertEqual(t5(Array(1, 2, 3, 4)), 3)

    assertEqual(
      test6().show,
      """((array1: scala.Array[scala.Int], array2: scala.Array[scala.Int]) => {
        |  var x: scala.Int = 0
        |  var x: scala.Int = array1.length
        |  var x: scala.Int = 0
        |  var x: scala.Int = 5
        |  while (x.>(0).&&(x.<(x))) {
        |    val el: scala.Int = array1.apply(x)
        |    x = x.+(1)
        |    var x: scala.Int = array2.length
        |    var x: scala.Int = 0
        |    var x: scala.Int = 2
        |    while (x.>(0).&&(x.>(0).&&(x.<(x)))) {
        |      val el: scala.Int = array2.apply(x)
        |      x = x.+(1)
        |      x = x.-(1)
        |      x = x.-(1)
        |      x = x.+(el)
        |    }
        |  }
        |  x
        |})""".stripMargin)
    val t6 = test6().run
    assertEqual(t6(Array(1, 1, 1), Array(1, 2, 3)), 7)
    assertEqual(t6(Array(1, 1, 1, 1), Array(1, 2, 3, 4)), 7)

    assertEqual(
      test7().show,
      """((array: scala.Array[scala.Int]) => {
        |  var x: scala.Int = 0
        |  var x: scala.Int = array.length
        |  var x: scala.Int = 0
        |  var x: scala.Int = array.length
        |  var x: scala.Int = 0
        |  while (x.<(x).&&(x.<(x))) {
        |    val el: scala.Int = array.apply(x)
        |    x = x.+(1)
        |    val el: scala.Int = array.apply(x)
        |    x = x.+(1)
        |    x = x.+(el.+(el))
        |  }
        |  x
        |})""".stripMargin)
    val t7 = test7().run
    assertEqual(t7(Array(1, 2, 3)), 12)
    assertEqual(t7(Array(1, 2, 3, 4)), 20)

    assertEqual(
      test8().show,
      """((array: scala.Array[scala.Int]) => {
        |  var x: scala.Int = 0
        |  var x: scala.Int = array.length
        |  var x: scala.Int = 0
        |  var x: scala.Int = array.length
        |  var x: scala.Int = 0
        |  var x: scala.Boolean = x.<(x)
        |  while (x.&&(x.<(x))) {
        |    val el: scala.Int = array.apply(x)
        |    x = x.+(1)
        |    var x: scala.Int = array.length
        |    var x: scala.Int = 0
        |    while (x.&&(x.<(x))) {
        |      val el: scala.Int = array.apply(x)
        |      x = x.+(1)
        |      val el: scala.Int = array.apply(x)
        |      x = x.+(1)
        |      x = x.+(el.+(el.+(el)))
        |      x = x.<(x)
        |    }
        |  }
        |  x
        |})""".stripMargin)
    val t8 = test8().run
    assertEqual(t8(Array(1, 2, 3)), 15)
    assertEqual(t8(Array(1, 2, 3, 4)), 24)

    assertEqual(
      test9().show,
      """((array: scala.Array[scala.Int]) => {
        |  var x: scala.Int = 0
        |  var x: scala.Int = array.length
        |  var x: scala.Int = 0
        |  var x: scala.Int = array.length
        |  var x: scala.Int = 0
        |  var x: scala.Boolean = x.<(x)
        |  while (x.&&(x.<(x))) {
        |    val el: scala.Int = array.apply(x)
        |    x = x.+(1)
        |    var x: scala.Int = array.length
        |    var x: scala.Int = 0
        |    while (x.&&(x.<(x))) {
        |      val el: scala.Int = array.apply(x)
        |      x = x.+(1)
        |      val el: scala.Int = array.apply(x)
        |      x = x.+(1)
        |      x = x.+(el.+(el).+(el))
        |      x = x.<(x)
        |    }
        |  }
        |  x
        |})""".stripMargin)
    val t9 = test9().run
    assertEqual(t9(Array(1, 2, 3)), 15)
    assertEqual(t9(Array(1, 2, 3, 4)), 24)

    assertEqual(
      test10().show,
      """((array: scala.Array[scala.Int]) => {
        |  var x: scala.Int = 0
        |  var x: scala.Int = array.length
        |  var x: scala.Int = 0
        |  var x: scala.Function1[scala.Unit, scala.Unit] = ((_$2: scala.Unit) => ())
        |  var x: scala.Boolean = true
        |  var x: scala.Int = {
        |    null.asInstanceOf[scala.Int]
        |  }
        |  def adv: scala.Function1[scala.Unit, scala.Unit] = ((_$3: scala.Unit) => {
        |    x = x.<(x)
        |    if (x) {
        |      val el: scala.Int = array.apply(x)
        |      x = x.+(1)
        |      var x: scala.Int = array.length
        |      var x: scala.Int = 0
        |      val oldnadv: scala.Function1[scala.Unit, scala.Unit] = x
        |      val adv1: scala.Function1[scala.Unit, scala.Unit] = ((_$1: scala.Unit) => if (x.<(x)) {
        |        val el: scala.Int = array.apply(x)
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
        |  var x: scala.Int = array.length
        |  var x: scala.Int = 0
        |  var x: scala.Boolean = x
        |  while (x.&&(x.<(x))) {
        |    val el: scala.Int = array.apply(x)
        |    x = x.+(1)
        |    var x: scala.Int = array.length
        |    var x: scala.Int = 0
        |    while (x.&&(x.<(x))) {
        |      val el: scala.Int = array.apply(x)
        |      x = x.+(1)
        |      var el: scala.Int = x
        |      val f: scala.Function1[scala.Unit, scala.Unit] = x
        |      f.apply(())
        |      x = x.+(el.+(el.+(el)))
        |      x = x
        |    }
        |  }
        |  x
        |})""".stripMargin)
    val t10 = test10().run
    assertEqual(t10(Array(1, 2, 3)), 72)
    assertEqual(t10(Array(1, 2, 3, 4)), 160)
  }

  private[this] var i = 1
  def assertEqual[T](actual: T, expected: T): Unit = {
    if (actual == expected) {
      println(s"Stymonas test $i ok")
      i += 1
    } else {
      println(s"Stymonas test $i: failed")
      println(s"Expeted:")
      println(expected)
      println(s"Actual:")
      println(actual)
      System.exit(1)
    }
  }
}

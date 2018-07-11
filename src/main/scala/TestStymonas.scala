import scala.quoted._

import strymonas._

object TestStymonas {
  implicit val toolbox: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make

def test1() = Stream
    .of('{Array(1, 2, 3)})
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))

  def test2() = Stream
    .of('{Array(1, 2, 3)})
    .map((a: Expr[Int]) => '{ ~a * 2 })
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))

  def test3() = Stream
    .of('{Array(1, 2, 3)})
    .flatMap((d: Expr[Int]) => Stream.of('{Array(1, 2, 3)}).map((dp: Expr[Int]) => '{ ~d * ~dp }))
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))

  def test4() = Stream
    .of('{Array(1, 2, 3)})
    .filter((d: Expr[Int]) => '{ ~d % 2 == 0 })
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))

  def test5() = Stream
    .of('{Array(1, 2, 3)})
    .take('{2})
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))

  def test6() = Stream
    .of('{Array(1, 1, 1)})
    .flatMap((d: Expr[Int]) => Stream.of('{Array(1, 2, 3)}).take('{2}))
    .take('{5})
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))

  def test7() = Stream
    .of('{Array(1, 2, 3)})
    .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ ~a + ~b }), Stream.of('{Array(1, 2, 3)}))
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))

  def test8() = Stream
    .of('{Array(1, 2, 3)})
    .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ ~a + ~b }), Stream.of('{Array(1, 2, 3)}).flatMap((d: Expr[Int]) => Stream.of('{Array(1, 2, 3)}).map((dp: Expr[Int]) => '{ ~d + ~dp })))
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))

  def test9() = Stream
    .of('{Array(1, 2, 3)}).flatMap((d: Expr[Int]) => Stream.of('{Array(1, 2, 3)}).map((dp: Expr[Int]) => '{ ~d + ~dp }))
    .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ ~a + ~b }), Stream.of('{Array(1, 2, 3)}) )
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))

  def test10() = Stream
    .of('{Array(1, 2, 3)}).flatMap((d: Expr[Int]) => Stream.of('{Array(1, 2, 3)}).map((dp: Expr[Int]) => '{ ~d + ~dp }))
    .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ ~a + ~b }), Stream.of('{Array(1, 2, 3)}).flatMap((d: Expr[Int]) => Stream.of('{Array(1, 2, 3)}).map((dp: Expr[Int]) => '{ ~d + ~dp })) )
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))

  def runTests(): Unit = {
    // TODO assert output
    assertEqual(
      test1().show,
      """{
        |  var x: scala.Int = 0
        |  var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |  var x: scala.Int = 0
        |  while (x.<(x)) {
        |    val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
        |    x = x.+(1)
        |    x = x.+(el)
        |  }
        |  x
        |}""".stripMargin)
    assertEqual(test1().run, 6)

    assertEqual(
      test2().show,
      """{
        |  var x: scala.Int = 0
        |  var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |  var x: scala.Int = 0
        |  while (x.<(x)) {
        |    val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
        |    x = x.+(1)
        |    x = x.+(el.*(2))
        |  }
        |  x
        |}""".stripMargin)
    assertEqual(test2().run, 12)

    assertEqual(
      test3().show,
      """{
        |  var x: scala.Int = 0
        |  var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |  var x: scala.Int = 0
        |  while (x.<(x)) {
        |    val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
        |    x = x.+(1)
        |    var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |    var x: scala.Int = 0
        |    while (x.<(x)) {
        |      val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
        |      x = x.+(1)
        |      x = x.+(el.*(el))
        |    }
        |  }
        |  x
        |}""".stripMargin)
    assertEqual(test3().run, 36)

    assertEqual(
      test4().show,
      """{
        |  var x: scala.Int = 0
        |  var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |  var x: scala.Int = 0
        |  while (x.<(x)) {
        |    val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
        |    x = x.+(1)
        |    if (el.%(2).==(0)) x = x.+(el) else ()
        |  }
        |  x
        |}""".stripMargin)
    assertEqual(test4().run, 2)

    assertEqual(
      test5().show,
      """{
        |  var x: scala.Int = 0
        |  var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |  var x: scala.Int = 0
        |  var x: scala.Int = 2
        |  while (x.>(0).&&(x.<(x))) {
        |    val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
        |    x = x.+(1)
        |    x = x.-(1)
        |    x = x.+(el)
        |  }
        |  x
        |}""".stripMargin)
    assertEqual(test5().run, 3)

    assertEqual(
      test6().show,
      """{
        |  var x: scala.Int = 0
        |  var x: scala.Int = scala.Array.apply(1, 1, 1).length
        |  var x: scala.Int = 0
        |  var x: scala.Int = 5
        |  while (x.>(0).&&(x.<(x))) {
        |    val el: scala.Int = scala.Array.apply(1, 1, 1).apply(x)
        |    x = x.+(1)
        |    var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |    var x: scala.Int = 0
        |    var x: scala.Int = 2
        |    while (x.>(0).&&(x.>(0).&&(x.<(x)))) {
        |      val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
        |      x = x.+(1)
        |      x = x.-(1)
        |      x = x.-(1)
        |      x = x.+(el)
        |    }
        |  }
        |  x
        |}""".stripMargin)
    assertEqual(test6().run, 7)

    assertEqual(
      test7().show,
      """{
        |  var x: scala.Int = 0
        |  var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |  var x: scala.Int = 0
        |  var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |  var x: scala.Int = 0
        |  while (x.<(x).&&(x.<(x))) {
        |    val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
        |    x = x.+(1)
        |    val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
        |    x = x.+(1)
        |    x = x.+(el.+(el))
        |  }
        |  x
        |}""".stripMargin)
    assertEqual(test7().run, 12)

    assertEqual(
      test8().show,
      """{
        |  var x: scala.Int = 0
        |  var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |  var x: scala.Int = 0
        |  var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |  var x: scala.Int = 0
        |  var x: scala.Boolean = x.<(x)
        |  while (x.&&(x.<(x))) {
        |    val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
        |    x = x.+(1)
        |    var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |    var x: scala.Int = 0
        |    while (x.&&(x.<(x))) {
        |      val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
        |      x = x.+(1)
        |      val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
        |      x = x.+(1)
        |      x = x.+(el.+(el.+(el)))
        |      x = x.<(x)
        |    }
        |  }
        |  x
        |}""".stripMargin)
    assertEqual(test8().run, 15)

    assertEqual(
      test9().show,
      """{
        |  var x: scala.Int = 0
        |  var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |  var x: scala.Int = 0
        |  var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |  var x: scala.Int = 0
        |  var x: scala.Boolean = x.<(x)
        |  while (x.&&(x.<(x))) {
        |    val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
        |    x = x.+(1)
        |    var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |    var x: scala.Int = 0
        |    while (x.&&(x.<(x))) {
        |      val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
        |      x = x.+(1)
        |      val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
        |      x = x.+(1)
        |      x = x.+(el.+(el).+(el))
        |      x = x.<(x)
        |    }
        |  }
        |  x
        |}""".stripMargin)
    assertEqual(test9().run, 15)

    assertEqual(
      test10().show,
      """{
        |  var x: scala.Int = 0
        |  var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |  var x: scala.Int = 0
        |  var x: scala.Function1[scala.Unit, scala.Unit] = ((_$5: scala.Unit) => ())
        |  var x: scala.Boolean = true
        |  var x: scala.Int = {
        |    null.asInstanceOf[scala.Int]
        |  }
        |  def adv: scala.Function1[scala.Unit, scala.Unit] = ((_$6: scala.Unit) => {
        |    x = x.<(x)
        |    if (x) {
        |      val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
        |      x = x.+(1)
        |      var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |      var x: scala.Int = 0
        |      val oldnadv: scala.Function1[scala.Unit, scala.Unit] = x
        |      val adv1: scala.Function1[scala.Unit, scala.Unit] = ((_$4: scala.Unit) => if (x.<(x)) {
        |        val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
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
        |  var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |  var x: scala.Int = 0
        |  var x: scala.Boolean = x
        |  while (x.&&(x.<(x))) {
        |    val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
        |    x = x.+(1)
        |    var x: scala.Int = scala.Array.apply(1, 2, 3).length
        |    var x: scala.Int = 0
        |    while (x.&&(x.<(x))) {
        |      val el: scala.Int = scala.Array.apply(1, 2, 3).apply(x)
        |      x = x.+(1)
        |      var el: scala.Int = x
        |      val f: scala.Function1[scala.Unit, scala.Unit] = x
        |      f.apply(())
        |      x = x.+(el.+(el.+(el)))
        |      x = x
        |    }
        |  }
        |  x
        |}""".stripMargin)
    assertEqual(test10().run, 72)
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

import scala.quoted._

import shonan.hmm._

object TestShonanHMM {
  implicit val toolbox: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make

  def runTests(): Unit = {
    {
      val intComplex = new RingComplex(RingInt)
      import intComplex._

      assertEqual(
        Complex(1, 2) * Complex(4, 2),
        Complex(0, 10))
    }

    {
      val intExprComplex = new RingComplex(RingIntExpr)
      import intExprComplex._

      val res = Complex('(1), '(2)) * Complex('(4), '(2))
      assertEqual(
        s"Complex(${res.re.show}, ${res.im.show})",
        "Complex(1.*(4).-(2.*(2)), 1.*(2).+(2.*(4)))")
    }

    val arr1 = Array(Complex(1, 0), Complex(0, 4), Complex(2, 2))
    val arr2 = Array(Complex(2, 0), Complex(1, 1), Complex(1, 2))
    val out  = Array(Complex(0, 0), Complex(0, 0), Complex(0, 0))
    Vmults.vmult(out, arr1, arr2)
    assertEqual(
      out.toList,
      List(Complex(2, 0), Complex(-4,4), Complex(-2,6)))

    assertEqual(
      Vmults.vmultCA.show,
      """((vout: scala.Array[shonan.hmm.Complex[scala.Int]], v1: scala.Array[shonan.hmm.Complex[scala.Int]], v2: scala.Array[shonan.hmm.Complex[scala.Int]]) => {
        |  val n: scala.Int = vout.length
        |  var i: scala.Int = 0
        |  while (i.<(n)) {
        |    vout.update(i, shonan.hmm.Complex.apply[scala.Int](v1.apply(i).re.*(v2.apply(i).re).-(v1.apply(i).im.*(v2.apply(i).im)), v1.apply(i).re.*(v2.apply(i).im).+(v1.apply(i).im.*(v2.apply(i).re))))
        |    i = i.+(1)
        |  }
        |})""".stripMargin)


    val a = Array(
      Array( 5,  0,  0,  5,  0),
      Array( 0,  0, 10,  0,  0),
      Array( 0, 10,  0,  0,  0),
      Array( 0,  0,  2,  3,  5),
      Array( 0,  0,  3,  0,  7)
    )

    val v1 = Array(1, 2, 3, 4, 5)
    val v1out = Array(0, 0, 0, 0, 0)
    MVmult.mvmult_p(v1out, a, v1)
    assertEqual(
      v1out.toList,
      List(25, 30, 20, 43, 44))

    assertEqual(
      MVmult.mvmult_c.show,
      """((vout: scala.Array[scala.Int], a: scala.Array[scala.Array[scala.Int]], v: scala.Array[scala.Int]) => {
        |  val n: scala.Int = vout.length
        |  val m: scala.Int = v.length
        |  var i: scala.Int = 0
        |  while (i.<(n)) {
        |    vout.update(i, {
        |      var sum: scala.Int = 0
        |      var i: scala.Int = 0
        |      while (i.<(m)) {
        |        sum = sum.+(v.apply(i).*(a.apply(i).apply(i)))
        |        i = i.+(1)
        |      }
        |      (sum: scala.Int)
        |    })
        |    i = i.+(1)
        |  }
        |})""".stripMargin)

    assertEqual(
      MVmult.mvmult_mc(3, 2).show,
      """((vout: scala.Array[scala.Int], a: scala.Array[scala.Array[scala.Int]], v: scala.Array[scala.Int]) => {
        |  if (3.!=(vout.length)) throw new scala.IndexOutOfBoundsException("3") else ()
        |  if (2.!=(v.length)) throw new scala.IndexOutOfBoundsException("2") else ()
        |  vout.update(0, 0.+(v.apply(0).*(a.apply(0).apply(0))).+(v.apply(1).*(a.apply(0).apply(1))))
        |  vout.update(1, 0.+(v.apply(0).*(a.apply(1).apply(0))).+(v.apply(1).*(a.apply(1).apply(1))))
        |  vout.update(2, 0.+(v.apply(0).*(a.apply(2).apply(0))).+(v.apply(1).*(a.apply(2).apply(1))))
        |})""".stripMargin)

    assertEqual(
      MVmult.mvmult_ac(a).show,
      """{
        |  val arr: scala.Array[scala.Array[scala.Int]] = {
        |    val array: scala.Array[scala.Array[scala.Int]] = dotty.runtime.Arrays.newGenericArray[scala.Array[scala.Int]](5)({
        |      scala.reflect.ClassTag.apply[scala.Array[scala.Int]](scala.Predef.classOf[scala.Array[scala.Int]])
        |    })
        |    array.update(0, {
        |      val array: scala.Array[scala.Int] = new scala.Array[scala.Int](5)
        |      array.update(0, 5)
        |      array.update(1, 0)
        |      array.update(2, 0)
        |      array.update(3, 5)
        |      array.update(4, 0)
        |      array
        |    })
        |    array.update(1, {
        |      val array: scala.Array[scala.Int] = new scala.Array[scala.Int](5)
        |      array.update(0, 0)
        |      array.update(1, 0)
        |      array.update(2, 10)
        |      array.update(3, 0)
        |      array.update(4, 0)
        |      array
        |    })
        |    array.update(2, {
        |      val array: scala.Array[scala.Int] = new scala.Array[scala.Int](5)
        |      array.update(0, 0)
        |      array.update(1, 10)
        |      array.update(2, 0)
        |      array.update(3, 0)
        |      array.update(4, 0)
        |      array
        |    })
        |    array.update(3, {
        |      val array: scala.Array[scala.Int] = new scala.Array[scala.Int](5)
        |      array.update(0, 0)
        |      array.update(1, 0)
        |      array.update(2, 2)
        |      array.update(3, 3)
        |      array.update(4, 5)
        |      array
        |    })
        |    array.update(4, {
        |      val array: scala.Array[scala.Int] = new scala.Array[scala.Int](5)
        |      array.update(0, 0)
        |      array.update(1, 0)
        |      array.update(2, 3)
        |      array.update(3, 0)
        |      array.update(4, 7)
        |      array
        |    })
        |    array
        |  }
        |
        |  ((vout: scala.Array[scala.Int], v: scala.Array[scala.Int]) => {
        |    if (5.!=(vout.length)) throw new scala.IndexOutOfBoundsException("5") else ()
        |    if (5.!=(v.length)) throw new scala.IndexOutOfBoundsException("5") else ()
        |    vout.update(0, 0.+(v.apply(0).*(5)).+(v.apply(1).*(0)).+(v.apply(2).*(0)).+(v.apply(3).*(5)).+(v.apply(4).*(0)))
        |    vout.update(1, 0.+(v.apply(0).*(0)).+(v.apply(1).*(0)).+(v.apply(2).*(10)).+(v.apply(3).*(0)).+(v.apply(4).*(0)))
        |    vout.update(2, 0.+(v.apply(0).*(0)).+(v.apply(1).*(10)).+(v.apply(2).*(0)).+(v.apply(3).*(0)).+(v.apply(4).*(0)))
        |    vout.update(3, 0.+(v.apply(0).*(0)).+(v.apply(1).*(0)).+(v.apply(2).*(2)).+(v.apply(3).*(3)).+(v.apply(4).*(5)))
        |    vout.update(4, 0.+(v.apply(0).*(0)).+(v.apply(1).*(0)).+(v.apply(2).*(3)).+(v.apply(3).*(0)).+(v.apply(4).*(7)))
        |  })
        |}""".stripMargin)

    assertEqual(
      MVmult.mvmult_opt(a).show,
      """{
        |  val arr: scala.Array[scala.Array[scala.Int]] = {
        |    val array: scala.Array[scala.Array[scala.Int]] = dotty.runtime.Arrays.newGenericArray[scala.Array[scala.Int]](5)({
        |      scala.reflect.ClassTag.apply[scala.Array[scala.Int]](scala.Predef.classOf[scala.Array[scala.Int]])
        |    })
        |    array.update(0, {
        |      val array: scala.Array[scala.Int] = new scala.Array[scala.Int](5)
        |      array.update(0, 5)
        |      array.update(1, 0)
        |      array.update(2, 0)
        |      array.update(3, 5)
        |      array.update(4, 0)
        |      array
        |    })
        |    array.update(1, {
        |      val array: scala.Array[scala.Int] = new scala.Array[scala.Int](5)
        |      array.update(0, 0)
        |      array.update(1, 0)
        |      array.update(2, 10)
        |      array.update(3, 0)
        |      array.update(4, 0)
        |      array
        |    })
        |    array.update(2, {
        |      val array: scala.Array[scala.Int] = new scala.Array[scala.Int](5)
        |      array.update(0, 0)
        |      array.update(1, 10)
        |      array.update(2, 0)
        |      array.update(3, 0)
        |      array.update(4, 0)
        |      array
        |    })
        |    array.update(3, {
        |      val array: scala.Array[scala.Int] = new scala.Array[scala.Int](5)
        |      array.update(0, 0)
        |      array.update(1, 0)
        |      array.update(2, 2)
        |      array.update(3, 3)
        |      array.update(4, 5)
        |      array
        |    })
        |    array.update(4, {
        |      val array: scala.Array[scala.Int] = new scala.Array[scala.Int](5)
        |      array.update(0, 0)
        |      array.update(1, 0)
        |      array.update(2, 3)
        |      array.update(3, 0)
        |      array.update(4, 7)
        |      array
        |    })
        |    array
        |  }
        |
        |  ((vout: scala.Array[scala.Int], v: scala.Array[scala.Int]) => {
        |    if (5.!=(vout.length)) throw new scala.IndexOutOfBoundsException("5") else ()
        |    if (5.!=(v.length)) throw new scala.IndexOutOfBoundsException("5") else ()
        |    vout.update(0, v.apply(0).*(5).+(v.apply(3).*(5)))
        |    vout.update(1, v.apply(2).*(10))
        |    vout.update(2, v.apply(1).*(10))
        |    vout.update(3, v.apply(2).*(2).+(v.apply(3).*(3)).+(v.apply(4).*(5)))
        |    vout.update(4, v.apply(2).*(3).+(v.apply(4).*(7)))
        |  })
        |}""".stripMargin)

    assertEqual(
      MVmult.mvmult_roll(a).show,
      """{
        |  val arr: scala.Array[scala.Array[scala.Int]] = {
        |    val array: scala.Array[scala.Array[scala.Int]] = dotty.runtime.Arrays.newGenericArray[scala.Array[scala.Int]](5)({
        |      scala.reflect.ClassTag.apply[scala.Array[scala.Int]](scala.Predef.classOf[scala.Array[scala.Int]])
        |    })
        |    array.update(0, {
        |      val array: scala.Array[scala.Int] = new scala.Array[scala.Int](5)
        |      array.update(0, 5)
        |      array.update(1, 0)
        |      array.update(2, 0)
        |      array.update(3, 5)
        |      array.update(4, 0)
        |      array
        |    })
        |    array.update(1, {
        |      val array: scala.Array[scala.Int] = new scala.Array[scala.Int](5)
        |      array.update(0, 0)
        |      array.update(1, 0)
        |      array.update(2, 10)
        |      array.update(3, 0)
        |      array.update(4, 0)
        |      array
        |    })
        |    array.update(2, {
        |      val array: scala.Array[scala.Int] = new scala.Array[scala.Int](5)
        |      array.update(0, 0)
        |      array.update(1, 10)
        |      array.update(2, 0)
        |      array.update(3, 0)
        |      array.update(4, 0)
        |      array
        |    })
        |    array.update(3, {
        |      val array: scala.Array[scala.Int] = new scala.Array[scala.Int](5)
        |      array.update(0, 0)
        |      array.update(1, 0)
        |      array.update(2, 2)
        |      array.update(3, 3)
        |      array.update(4, 5)
        |      array
        |    })
        |    array.update(4, {
        |      val array: scala.Array[scala.Int] = new scala.Array[scala.Int](5)
        |      array.update(0, 0)
        |      array.update(1, 0)
        |      array.update(2, 3)
        |      array.update(3, 0)
        |      array.update(4, 7)
        |      array
        |    })
        |    array
        |  }
        |
        |  ((vout: scala.Array[scala.Int], v: scala.Array[scala.Int]) => {
        |    if (5.!=(vout.length)) throw new scala.IndexOutOfBoundsException("5") else ()
        |    if (5.!=(v.length)) throw new scala.IndexOutOfBoundsException("5") else ()
        |    vout.update(0, v.apply(0).*(5).+(v.apply(3).*(5)))
        |    vout.update(1, v.apply(2).*(10))
        |    vout.update(2, v.apply(1).*(10))
        |    vout.update(3, {
        |      var sum: scala.Int = 0
        |      var i: scala.Int = 0
        |      while (i.<(5)) {
        |        sum = sum.+(v.apply(i).*(arr.apply(3).apply(i)))
        |        i = i.+(1)
        |      }
        |      (sum: scala.Int)
        |    })
        |    vout.update(4, v.apply(2).*(3).+(v.apply(4).*(7)))
        |  })
        |}""".stripMargin)

    assertEqual(
      MVmult.mvmult_let1(a).show,
      """((vout: scala.Array[scala.Int], v: scala.Array[scala.Int]) => {
        |  if (5.!=(vout.length)) throw new scala.IndexOutOfBoundsException("5") else ()
        |  if (5.!=(v.length)) throw new scala.IndexOutOfBoundsException("5") else ()
        |  vout.update(0, v.apply(0).*(5).+(v.apply(3).*(5)))
        |  vout.update(1, v.apply(2).*(10))
        |  vout.update(2, v.apply(1).*(10))
        |  vout.update(3, {
        |    var sum: scala.Int = 0
        |    var i: scala.Int = 0
        |    while (i.<(5)) {
        |      sum = sum.+(v.apply(i).*({
        |        val array: scala.Array[scala.Int] = new scala.Array[scala.Int](5)
        |        array.update(0, 0)
        |        array.update(1, 0)
        |        array.update(2, 2)
        |        array.update(3, 3)
        |        array.update(4, 5)
        |        array
        |      }.apply(i)))
        |      i = i.+(1)
        |    }
        |    (sum: scala.Int)
        |  })
        |  vout.update(4, v.apply(2).*(3).+(v.apply(4).*(7)))
        |})""".stripMargin)

    assertEqual(
      MVmult.mvmult_let(a).show,
      """{
        |  val row: scala.Array[scala.Int] = {
        |    val array: scala.Array[scala.Int] = new scala.Array[scala.Int](5)
        |    array.update(0, 0)
        |    array.update(1, 0)
        |    array.update(2, 2)
        |    array.update(3, 3)
        |    array.update(4, 5)
        |    array
        |  }
        |
        |  ((vout: scala.Array[scala.Int], v: scala.Array[scala.Int]) => {
        |    if (5.!=(vout.length)) throw new scala.IndexOutOfBoundsException("5") else ()
        |    if (5.!=(v.length)) throw new scala.IndexOutOfBoundsException("5") else ()
        |    vout.update(0, v.apply(0).*(5).+(v.apply(3).*(5)))
        |    vout.update(1, v.apply(2).*(10))
        |    vout.update(2, v.apply(1).*(10))
        |    vout.update(3, {
        |      var sum: scala.Int = 0
        |      var i: scala.Int = 0
        |      while (i.<(5)) {
        |        sum = sum.+(v.apply(i).*(row.apply(i)))
        |        i = i.+(1)
        |      }
        |      (sum: scala.Int)
        |    })
        |    vout.update(4, v.apply(2).*(3).+(v.apply(4).*(7)))
        |  })
        |}""".stripMargin)
  }

  private[this] var i = 1
  def assertEqual[T](actual: T, expected: T): Unit = {
    if (actual == expected) {
      println(s"Shonan HMM test $i ok")
      i += 1
    } else {
      println(s"Shonan HMM test $i: failed")
      println(s"Expeted:")
      println(expected)
      println(s"Actual:")
      println(actual)
      System.exit(1)
    }
  }
}

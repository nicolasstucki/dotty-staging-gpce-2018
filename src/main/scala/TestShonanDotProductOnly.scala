import scala.quoted._

import shonan.dot._

object TestShonanDotProductOnly {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  def runTests(): Unit = {
    val arr1 = Array(0, 1, 2, 4, 8)
    val arr2 = Array(1, 0, 1, 0, 1)
    val cmpxArr1 = Array(Complex(1, 0), Complex(2, 3), Complex(0, 2), Complex(3, 1))
    val cmpxArr2 = Array(Complex(0, 1), Complex(0, 0), Complex(0, 1), Complex(2, 0))

    val vec1 = new Vec(arr1.size, i => arr1(i))
    val vec2 = new Vec(arr2.size, i => arr2(i))
    val cmpxVec1 = new Vec(cmpxArr1.size, i => cmpxArr1(i))
    val cmpxVec2 = new Vec(cmpxArr2.size, i => cmpxArr2(i))

    val blasInt = new Blas1(new RingInt, new StaticVecOps)
    assertEqual(blasInt.dot(vec1, vec2), 10)

    val blasComplexInt = new Blas1(new RingComplex(new RingInt), new StaticVecOps)
    assertEqual(blasComplexInt.dot(cmpxVec1, cmpxVec2), Complex(4,3))

    val blasStaticIntExpr = new Blas1(new RingIntExpr, new StaticVecOps)
    val resCode1 = blasStaticIntExpr.dot(
      vec1.map(_.toExpr),
      vec2.map(_.toExpr)
    )
    assertEqual(resCode1.show, "0.+(0.*(1)).+(1.*(0)).+(2.*(1)).+(4.*(0)).+(8.*(1))")
    assertEqual(resCode1.run, 10)

    val blasExprIntExpr = new Blas1(new RingIntExpr, new ExprVecOps)
    val resCode2: Expr[(Array[Int], Array[Int]) => Int] = '{
      (arr1, arr2) =>
        if (arr1.length != arr2.length) throw new Exception("...")
        ~{
          blasExprIntExpr.dot(
            new Vec('(arr1.size), i => '(arr1(~i))),
            new Vec('(arr2.size), i => '(arr2(~i)))
          )
        }
    }
    assertEqual(
      resCode2.show,
      """((arr1: scala.Array[scala.Int], arr2: scala.Array[scala.Int]) => {
        |  if (arr1.length.!=(arr2.length)) throw new scala.Exception("...") else ()
        |  var sum: scala.Int = 0
        |  var i: scala.Int = 0
        |  while (i.<(scala.Predef.intArrayOps(arr1).size)) {
        |    sum = sum.+(arr1.apply(i).*(arr2.apply(i)))
        |    i = i.+(1)
        |  }
        |  (sum: scala.Int)
        |})""".stripMargin)
    assertEqual(resCode2.run.apply(arr1, arr2), 10)

    val blasStaticIntPVExpr = new Blas1(new RingPV[Int](new RingInt, new RingIntExpr), new StaticVecOps)
    val resCode3 = blasStaticIntPVExpr.dot(
      vec1.map(i => Dyn(i.toExpr)),
      vec2.map(i => Sta(i))
    ).expr
    assertEqual(resCode3.show, "0.+(2).+(8)")
    assertEqual(resCode3.run, 10)

    val blasExprIntPVExpr = new Blas1(new RingPV[Int](new RingInt, new RingIntExpr), new StaticVecOps)
    val resCode4: Expr[Array[Int] => Int] = '{
      arr =>
        if (arr.length != ~vec2.size.toExpr) throw new Exception("...")
        ~{
          blasExprIntPVExpr.dot(
            new Vec(vec2.size, i => Dyn('(arr(~i.toExpr)))),
            vec2.map(i => Sta(i))
          ).expr
        }

    }
    assertEqual(
      resCode4.show,
      """((arr: scala.Array[scala.Int]) => {
        |  if (arr.length.!=(5)) throw new scala.Exception("...") else ()
        |  arr.apply(0).+(arr.apply(2)).+(arr.apply(4))
        |})""".stripMargin)
    assertEqual(resCode4.run.apply(arr1), 10)

    import Complex.isLiftable
    val blasExprComplexPVInt = new Blas1[Int, Complex[PV[Int]]](new RingComplex(new RingPV[Int](new RingInt, new RingIntExpr)), new StaticVecOps)
    val resCode5: Expr[Array[Complex[Int]] => Complex[Int]] = '{
      arr =>
        if (arr.length != ~cmpxVec2.size.toExpr) throw new Exception("...")
        ~{
          val cpx = blasExprComplexPVInt.dot(
            new Vec(cmpxVec2.size, i => Complex(Dyn('(arr(~i.toExpr).re)), Dyn('(arr(~i.toExpr).im)))),
            new Vec(cmpxVec2.size, i => Complex(Sta(cmpxVec2.get(i).re), Sta(cmpxVec2.get(i).im)))
          )
          '(Complex(~cpx.re.expr, ~cpx.im.expr))
        }
    }
    assertEqual(
      resCode5.show,
      """((arr: scala.Array[shonan.dot.Complex[scala.Int]]) => {
        |  if (arr.length.!=(4)) throw new scala.Exception("...") else ()
        |  shonan.dot.Complex.apply[scala.Int](0.-(arr.apply(0).im).+(0.-(arr.apply(2).im)).+(arr.apply(3).re.*(2)), arr.apply(0).re.+(arr.apply(2).re).+(arr.apply(3).im.*(2)))
        |})""".stripMargin)
    assertEqual(resCode5.run.apply(cmpxArr1), Complex(4,3))

    val RingPVInt = new RingPV[Int](new RingInt, new RingIntExpr)
    // Staged loop of dot product on vectors of Int or Expr[Int]
    val dotIntOptExpr = new Blas1(RingPVInt, new StaticVecOps).dot
    // will generate the code '{ ((arr: scala.Array[scala.Int]) => arr.apply(1).+(arr.apply(3))) }
    val staticVec = Vec[Int, PV[Int]](5, i => Sta((i % 2)))
    val code = '{(arr: Array[Int]) => ~dotIntOptExpr(Vec(5, i => Dyn('(arr(~i.toExpr)))), staticVec).expr }
    assertEqual(code.show, "((arr: scala.Array[scala.Int]) => arr.apply(1).+(arr.apply(3)))")
  }

  private[this] var i = 1
  def assertEqual[T](actual: T, expected: T): Unit = {
    if (actual == expected) {
      println(s"Dot product test $i ok")
      i += 1
    } else {
      println(s"Dot product test $i: failed")
      println(s"Expeted:")
      println(expected)
      println(s"Actual:")
      println(actual)
      System.exit(1)
    }
  }
}

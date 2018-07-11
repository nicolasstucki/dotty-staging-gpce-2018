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
    println(test1().show)
    println(test1().run)
    println
    println(test2().show)
    println(test2().run)
    println
    println(test3().show)
    println(test3().run)
    println
    println(test4().show)
    println(test4().run)
    println
    println(test5().show)
    println(test5().run)
    println
    println(test6().show)
    println(test6().run)
    println
    println(test7().show)
    println(test7().run)
    println
    println(test8().show)
    println(test8().run)
    println
    println(test9().show)
    println(test9().run)
    println
    println(test10().show)
    println(test10().run)
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

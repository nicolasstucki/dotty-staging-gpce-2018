package snippets

import scala.quoted._

object Section3 {
  implicit val toolbox: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make

  def staged[T](arr: Expr[Array[T]], f: Expr[T] => Expr[Unit])(implicit t: Type[T]): Expr[Unit] = '{
    var i: Int = 0
    while (i < (~arr).length) {
      val element: ~t = (~arr)(i)
      ~f('(element))
      i += 1
    }
  }

  def sumCodeFor(arr: Expr[Array[Int]]): Expr[Int] = '{
    var sum = 0
    ~staged(arr, x => '(sum += ~x))
    sum
  }
  val sumCode = '{ (arr: Array[Int]) => ~sumCodeFor('(arr)) }

  // evaluate the code of sumCode which return the lambda
  val sum: Array[Int] => Int = sumCode.run
  sum(Array(1, 2, 3)) // Returns 6
  sum(Array(2, 3, 4, 5)) // Returns 14

}

object Section3Macros {

  inline def sum(arr: Array[Int]): Int = ~Section3.sumCodeFor('(arr))

  inline def sumN(inline size: Int, arr: Array[Int]): Int =
    ~Section4.sumNCode(size, '(arr))

}

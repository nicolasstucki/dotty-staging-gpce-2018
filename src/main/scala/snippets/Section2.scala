package snippets

import scala.quoted._

object Section2 {
  def unrolled(list: List[Expr[Int]], f: Expr[Int] => Expr[Unit]): Expr[Unit] = list match {
    case head :: tail => '{ ~f(head); ~unrolled(tail, f) }
    case Nil => '()
  }
  def unrolledExample1 =
    unrolled(List('(1), '(2)), (i: Expr[Int]) => '(println(~i)))

  def some[T](x: Expr[T], t:Type[T]): Expr[Some[T]] =
    '{ Some[~t](~x) }

  def someType[T](t:Type[T]): Type[Some[T]] =
    '[Some[~t]]

}

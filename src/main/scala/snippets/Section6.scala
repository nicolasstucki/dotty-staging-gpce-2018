package snippets

import scala.quoted._

object Section6 {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  inline def foreach(arr: Array[Int], f: Int => Unit): Unit = ~Section5.staged('(arr), '(f))

  def reflect[T: Type, U: Type](f: Expr[T] => Expr[U]): Expr[T => U] =
   '{ (x: T) => ~f('(x)) }
}

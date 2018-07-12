package snippets

import scala.quoted._

object Section6 {
  implicit val toolbox: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make

  inline def foreach(arr: Array[Int], f: Int => Unit): Unit = ~Section5.staged('(arr), x => ('(f))(x))

  def reflect[T: Type, U: Type](f: Expr[T] => Expr[U]): Expr[T => U] =
   '{ (x: T) => ~f('(x)) }
}

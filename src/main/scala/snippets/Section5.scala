package snippets

import scala.quoted._

object Section5 {
  implicit val toolbox: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make

  def staged[T: Type](arr: Expr[Array[T]], f: Expr[T] => Expr[Unit]): Expr[Unit] = '{
    var i = 0
    while (i < (~arr).length) {
      val element: T = (~arr)(i)
      ~f('(element))
      i += 1
    }
  }

}

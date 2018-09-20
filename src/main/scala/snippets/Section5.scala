package snippets

import scala.quoted._

object Section5 {

  def staged[T: Type](arr: Expr[Array[T]], f: Expr[T => Unit]): Expr[Unit] = '{
    var i = 0
    while (i < (~arr).length) {
      val element: T = (~arr)(i)
      ~f('(element))
      i += 1
    }
  }

}

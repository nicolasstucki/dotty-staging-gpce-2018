package snippets

import scala.quoted._

object Section4 {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  def sumNCode(size: Int, arr: Expr[Array[Int]]): Expr[Int] = '{
    assert((~arr).length == ~size.toExpr)
    var sum = 0
    ~Section2.unrolled(List.tabulate(size)(_.toExpr),
        x => '(sum += (~arr)(~x)))
    sum
  }

  implicit def BooleanIsLiftable: Liftable[Boolean] = new {
    def toExpr(b: Boolean): Expr[Boolean] =
      if (b) '(true) else '(false)
  }

  implicit def IntIsLiftable: Liftable[Int] = new {
    def toExpr(n: Int): Expr[Int] = n match {
      case Int.MinValue    => '(Int.MinValue)
      case _ if n < 0      => '(-(~toExpr(n)))
      case 0               => '(0)
      case _ if n % 2 == 0 => '(~toExpr(n / 2) * 2)
      case _               => '(~toExpr(n / 2) * 2 + 1)
    }
  }

  implicit def ListIsLiftable[T: Liftable: Type]: Liftable[List[T]] = new {
    def toExpr(xs: List[T]): Expr[List[T]] = xs match {
      case x :: xs1 => '(~x.toExpr :: ~toExpr(xs1))
      case Nil => '(Nil: List[T])
    }
  }

}

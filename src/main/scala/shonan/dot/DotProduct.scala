package shonan.dot

import scala.quoted._

trait Ring[T] {
  val zero: T
  val one: T
  val add: (x: T, y: T) => T
  val sub: (x: T, y: T) => T
  val mul: (x: T, y: T) => T
}

class RingInt extends Ring[Int] {
  val zero = 0
  val one  = 1
  val add  = (x, y) => x + y
  val sub  = (x, y) => x - y
  val mul  = (x, y) => x * y
}

class RingIntExpr extends Ring[Expr[Int]] {
  val zero = '(0)
  val one  = '(1)
  val add  = (x, y) => '(~x + ~y)
  val sub  = (x, y) => '(~x - ~y)
  val mul  = (x, y) => '(~x * ~y)
}

class RingComplex[U](u: Ring[U]) extends Ring[Complex[U]] {
  val zero = Complex(u.zero, u.zero)
  val one  = Complex(u.one, u.zero)
  val add  = (x, y) => Complex(u.add(x.re, y.re), u.add(x.im, y.im))
  val sub  = (x, y) => Complex(u.sub(x.re, y.re), u.sub(x.im, y.im))
  val mul  = (x, y) => Complex(u.sub(u.mul(x.re, y.re), u.mul(x.im, y.im)), u.add(u.mul(x.re, y.im), u.mul(x.im, y.re)))
}

sealed trait PV[T] {
  def expr(implicit l: Liftable[T]): Expr[T]
}
case class Sta[T](x: T) extends PV[T] {
  def expr(implicit l: Liftable[T]): Expr[T] = x.toExpr
}
case class Dyn[T](x: Expr[T]) extends PV[T] {
  def expr(implicit l: Liftable[T]): Expr[T] = x
}

class RingPV[U: Liftable](u: Ring[U], eu: Ring[Expr[U]]) extends Ring[PV[U]] {
  val zero: PV[U] = Sta(u.zero)
  val one: PV[U] = Sta(u.one)
  val add = (x: PV[U], y: PV[U]) => (x, y) match {
    case (Sta(u.zero), x) => x
    case (x, Sta(u.zero)) => x
    case (Sta(x), Sta(y)) => Sta(u.add(x, y))
    case (x, y) => Dyn(eu.add(x.expr, y.expr))
  }
  val sub = (x: PV[U], y: PV[U]) => (x, y) match {
    case (x, Sta(u.zero)) => x
    case (Sta(x), Sta(y)) => Sta(u.sub(x, y))
    case (x, y) => Dyn(eu.sub(x.expr, y.expr))
  }
  val mul = (x: PV[U], y: PV[U]) => (x, y) match {
    case (Sta(u.zero), _) => Sta(u.zero)
    case (_, Sta(u.zero)) => Sta(u.zero)
    case (Sta(u.one), x) => x
    case (x, Sta(u.one)) => x
    case (Sta(x), Sta(y)) => Sta(u.mul(x, y))
    case (x, y) => Dyn(eu.mul(x.expr, y.expr))
  }
}


case class Complex[T](re: T, im: T)

object Complex {
  implicit def isLiftable[T: Type: Liftable]: Liftable[Complex[T]] = new Liftable[Complex[T]] {
    def toExpr(comp: Complex[T]): Expr[Complex[T]] = '(Complex(~comp.re.toExpr, ~comp.im.toExpr))
  }
}

case class Vec[Idx, T](size: Idx, get: Idx => T) {
  def map[U](f: T => U): Vec[Idx, U] = Vec(size, i => f(get(i)))
  def zipWith[U, V](other: Vec[Idx, U], f: (T, U) => V): Vec[Idx, V] = Vec(size, i => f(get(i), other.get(i)))
}

object Vec {
  def from[T](elems: T*): Vec[Int, T] = new Vec(elems.size, i => elems(i))
}

trait VecOps[Idx, T] {
  val reduce: ((T, T) => T, T, Vec[Idx, T]) => T
}

class StaticVecOps[T] extends VecOps[Int, T] {
  val reduce: ((T, T) => T, T, Vec[Int, T]) => T = (plus, zero, vec) => {
    var sum = zero
    for (i <- 0 until vec.size)
      sum = plus(sum, vec.get(i))
    sum
  }
}

class ExprVecOps[T: Type] extends VecOps[Expr[Int], Expr[T]] {
  val reduce: ((Expr[T], Expr[T]) => Expr[T], Expr[T], Vec[Expr[Int], Expr[T]]) => Expr[T] = (plus, zero, vec) => '{
    var sum = ~zero
    var i = 0
    while (i < ~vec.size) {
      sum = ~{ plus('(sum), vec.get('(i))) }
      i += 1
    }
    sum
  }
}

class Blas1[Idx, T](r: Ring[T], ops: VecOps[Idx, T]) {
  def dot(v1: Vec[Idx, T], v2: Vec[Idx, T]): T = ops.reduce(r.add, r.zero, v1.zipWith(v2, r.mul))
}

package strymonas

import strymonas.{Stream => SStream}
import scala.quoted._

object TestPipelines {

 def sum() = '{ (array: Array[Int]) =>
    ~Stream.of('(array))
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def sumOfSquares() = '{ (array: Array[Int]) =>
    ~Stream.of('(array))
      .map((a: Expr[Int]) => '{ ~a * ~a })
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def sumOfSquaresEven() = '{ (array: Array[Int]) =>
    ~Stream.of('(array))
      .filter((d: Expr[Int]) => '{ ~d % 2 == 0 })
      .map((a: Expr[Int]) => '{ ~a * ~a })
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def cart() = '{ (vHi: Array[Int], vLo: Array[Int]) =>
    ~Stream.of('(vHi))
      .flatMap((d: Expr[Int]) => Stream.of('(vLo)).map((dp: Expr[Int]) => '{ ~d * ~dp }))
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def filter() = '{ (array: Array[Int]) =>
    ~Stream.of('(array))
      .filter((d: Expr[Int]) => '{ ~d % 2 == 0 })
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def take() = '{ (array: Array[Int]) =>
    ~Stream.of('(array))
      .take('{2})
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def flatMap_take() = '{ (array1: Array[Int], array2: Array[Int]) =>
    ~Stream.of('(array1))
      .flatMap((d: Expr[Int]) => Stream.of('(array2)))
      .take('{20000000})
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def dotProduct() = '{ (array1: Array[Int], array2: Array[Int])  =>
    ~Stream.of('(array1))
      .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ ~a + ~b }), Stream.of('(array2)))
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def flatMap_after_zip() = '{ (array1: Array[Int], array2: Array[Int]) =>
    ~Stream.of('(array1))
      .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ ~a + ~b }), Stream.of('(array1)))
      .flatMap((d: Expr[Int]) => Stream.of('(array2)).map((dp: Expr[Int]) => '{ ~d + ~dp }))
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def zip_after_flatMap() = '{ (array1: Array[Int], array2: Array[Int]) =>
    ~Stream.of('(array1))
      .flatMap((d: Expr[Int]) => Stream.of('(array2)).map((dp: Expr[Int]) => '{ ~d + ~dp }))
      .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ ~a + ~b }), Stream.of('(array1)) )
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

  def zip_flat_flat() = '{ (array1: Array[Int], array2: Array[Int]) =>
    ~Stream.of('(array1))
      .flatMap((d: Expr[Int]) => Stream.of('(array2)).map((dp: Expr[Int]) => '{ ~d + ~dp }))
      .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ ~a + ~b }), Stream.of('(array2)).flatMap((d: Expr[Int]) => Stream.of('(array1)).map((dp: Expr[Int]) => '{ ~d + ~dp })) )
      .take('(20000000))
      .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))
  }

}
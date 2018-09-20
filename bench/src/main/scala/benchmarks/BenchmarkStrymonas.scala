package benchmarks

import strymonas.{Stream => SStream}
import scala.quoted._
import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ArrayBuffer

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(Scope.Thread)
@Measurement(iterations = 30)
@Warmup(30)
@Fork(3)
class S {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make
  import strymonas.TestPipelines._
  import S._

  var N : Int = 100000000

  var v : Array[Int] = _
  var vHi : Array[Int] = _
  var vLo : Array[Int] = _
  var vFaZ : Array[Int] = _
  var vZaF : Array[Int] = _

  @Setup(Level.Trial)
  def prepare() : Unit = {
    v          = Array.tabulate(N)(i => i.toInt % 10)
    vHi        = Array.tabulate(10000000)(i => i.toInt % 10)
    vLo        = Array.tabulate(10)(i => i.toInt % 10)
    vFaZ       = Array.tabulate(10000)(_.toInt)
    vZaF       = Array.tabulate(10000000)(_.toInt)

    sumS = sum().run
    sumOfSquaresS = sumOfSquares().run
    sumOfSquaresEvenS = sumOfSquaresEven().run
    cartS = cart().run
    dotProductS = dotProduct().run
    flatMap_after_zipS = flatMap_after_zip().run
    flatMap_takeS = flatMap_take().run
    zip_after_flatMapS = zip_after_flatMap().run
    zip_flat_flatS = zip_flat_flat().run
  }

  var sumS                  : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
  var sumOfSquaresS         : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
  var sumOfSquaresEvenS     : Array[Int] => Int = null.asInstanceOf[Array[Int] => Int]
  var cartS                 : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
  var dotProductS           : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
  var flatMap_takeS         : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
  var flatMap_after_zipS    : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
  var zip_after_flatMapS    : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]
  var zip_flat_flatS        : (Array[Int], Array[Int]) => Int = null.asInstanceOf[(Array[Int], Array[Int]) => Int]

  @Benchmark
  def sum_staged_no_init () : Int = {
    val res : Int = sumS(v)
    res
  }

  @Benchmark
  def sum_staged_with_init () : Int = {
    val sumS = sum().run
    val res : Int = sumS(v)
    res
  }

  @Benchmark
  def sum_macro() : Int = {
    val res : Int = sumMacro(v)
    res
  }

  @Benchmark
  def sum_staged_init () : Unit = {
    sum().run
  }

  @Benchmark
  def sum_staged_init_fresh_compiler () : Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make
    sum().run
  }

  @Benchmark
  def sumOfSquares_staged_no_init () : Int = {
    val res : Int = sumOfSquaresS(v)
    res
  }

  @Benchmark
  def sumOfSquares_macro () : Int = {
    val res : Int = sumOfSquaresMacro(v)
    res
  }

  @Benchmark
  def sumOfSquares_staged_with_init () : Int = {
    val sumOfSquaresS = sumOfSquares().run
    val res : Int = sumOfSquaresS(v)
    res
  }

  @Benchmark
  def sumOfSquares_staged_init () : Unit = {
    sumOfSquares().run
  }

  @Benchmark
  def sumOfSquares_staged_init_fresh_compiler () : Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make
    sumOfSquares().run
  }

  @Benchmark
  def sumOfSquaresEven_staged_no_init () : Int = {
    val res : Int = sumOfSquaresEvenS(v)
    res
  }

  @Benchmark
  def sumOfSquaresEven_macro () : Int = {
    val res : Int = sumOfSquaresEvenMacro(v)
    res
  }

  @Benchmark
  def sumOfSquaresEven_staged_with_init () : Int = {
    val sumOfSquaresEvenS = sumOfSquaresEven().run
    val res : Int = sumOfSquaresEvenS(v)
    res
  }

  @Benchmark
  def sumOfSquaresEven_staged_init () : Unit = {
    sumOfSquaresEven().run
  }

  @Benchmark
  def sumOfSquaresEven_staged_init_fresh_compiler () : Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make
    sumOfSquaresEven().run
  }

  @Benchmark
  def cart_staged_no_init () : Int = {
    val res : Int = cartS(vHi, vLo)
    res
  }

  @Benchmark
  def cart_staged_macro () : Int = {
    val res : Int = cartMacro(vHi, vLo)
    res
  }

  @Benchmark
  def cart_staged_with_init () : Int = {
    val cartS = cart().run
    val res : Int = cartS(vHi, vLo)
    res
  }

  @Benchmark
  def cart_staged_init () : Unit = {
    cart().run
  }

  @Benchmark
  def cart_staged_init_fresh_compiler () : Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make
    cart().run
  }

  @Benchmark
  def dotProduct_staged_no_init () : Int = {
    val res : Int = dotProductS(vHi, vHi)
    res
  }

  @Benchmark
  def dotProduct_macro () : Int = {
    val res : Int = dotProductMacro(vHi, vHi)
    res
  }

  @Benchmark
  def dotProduct_staged_with_init () : Int = {
    val dotProductS = dotProduct().run
    val res : Int = dotProductS(vHi, vHi)
    res
  }

  @Benchmark
  def dotProduct_staged_init () : Unit = {
    dotProduct().run
  }

  @Benchmark
  def dotProduct_staged_init_fresh_compiler () : Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make
    dotProduct().run
  }

  @Benchmark
  def flatMap_after_zip_staged_no_init () : Int = {
    val res : Int = flatMap_after_zipS(vFaZ, vFaZ)
    res
  }

  @Benchmark
  def flatMap_after_zip_macro () : Int = {
    val res : Int = flatMap_after_zipMacro(vFaZ, vFaZ)
    res
  }

  @Benchmark
  def flatMap_after_zip_staged_with_init () : Int = {
    val flatMap_after_zipS = flatMap_after_zip().run
    val res : Int = flatMap_after_zipS(vFaZ, vFaZ)
    res
  }

  @Benchmark
  def flatMap_after_zip_staged_init () : Unit = {
    flatMap_after_zip().run
  }

  @Benchmark
  def flatMap_after_zip_staged_init_fresh_compiler () : Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make
    flatMap_after_zip().run
  }

  @Benchmark
  def zip_after_flatMap_staged_no_init () : Int = {
    val res : Int = zip_after_flatMapS(vZaF, vZaF)
    res
  }

  @Benchmark
  def zip_after_flatMap_macro () : Int = {
    val res : Int = zip_after_flatMapMacro(vZaF, vZaF)
    res
  }

  @Benchmark
  def zip_after_flatMap_staged_with_init () : Int = {
    val zip_after_flatMapS = zip_after_flatMap().run
    val res : Int = zip_after_flatMapS(vZaF, vZaF)
    res
  }

  @Benchmark
  def zip_after_flatMap_staged_init () : Unit = {
    flatMap_take().run
  }

  @Benchmark
  def zip_after_flatMap_staged_init_fresh_compiler () : Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make
    flatMap_take().run
  }

  @Benchmark
  def flatMap_take_staged_no_init () : Int = {
    val res : Int = flatMap_takeS(v, vLo)
    res
  }

  @Benchmark
  def flatMap_take_macro () : Int = {
    val res : Int = flatMap_takeMacro(v, vLo)
    res
  }

  @Benchmark
  def flatMap_take_staged_with_init () : Int = {
    val flatMap_takeS = flatMap_take().run
    val res : Int = flatMap_takeS(v, vLo)
    res
  }

  @Benchmark
  def flatMap_take_staged_init () : Unit = {
    zip_after_flatMap().run
  }

  @Benchmark
  def flatMap_take_staged_init_fresh_compiler () : Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make
    zip_after_flatMap().run
  }

  @Benchmark
  def zip_flat_flat_staged_no_init () : Int = {
    val res : Int = zip_flat_flatS(v, vLo)
    res
  }

  @Benchmark
  def zip_flat_flat_macro () : Int = {
    val res : Int = zip_flat_flatMacro(v, vLo)
    res
  }

  @Benchmark
  def zip_flat_flat_staged_with_init () : Int = {
    val zip_flat_flatS = zip_flat_flat().run
    val res : Int = zip_flat_flatS(v, vLo)
    res
  }

  @Benchmark
  def zip_flat_flat_staged_init () : Unit = {
    zip_flat_flat().run
  }

  @Benchmark
  def zip_flat_flat_staged_init_fresh_compiler () : Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make
    zip_flat_flat().run
  }
}

object S {
  inline def sumMacro: Array[Int] => Int = ~strymonas.TestPipelines.sum()
  inline def sumOfSquaresMacro: Array[Int] => Int = ~strymonas.TestPipelines.sumOfSquares()
  inline def sumOfSquaresEvenMacro: Array[Int] => Int = ~strymonas.TestPipelines.sumOfSquaresEven()
  inline def cartMacro: (Array[Int], Array[Int]) => Int = ~strymonas.TestPipelines.cart()
  inline def dotProductMacro: (Array[Int], Array[Int]) => Int = ~strymonas.TestPipelines.dotProduct()
  inline def flatMap_after_zipMacro: (Array[Int], Array[Int]) => Int = ~strymonas.TestPipelines.flatMap_after_zip()
  inline def zip_after_flatMapMacro: (Array[Int], Array[Int]) => Int = ~strymonas.TestPipelines.zip_after_flatMap()
  inline def flatMap_takeMacro: (Array[Int], Array[Int]) => Int = ~strymonas.TestPipelines.flatMap_take()
  inline def zip_flat_flatMacro: (Array[Int], Array[Int]) => Int = ~strymonas.TestPipelines.zip_flat_flat()
}
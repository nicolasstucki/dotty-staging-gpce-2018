package benchmarks

import strymonas.{Stream => SStream}
import scala.quoted._
import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ArrayBuffer

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(Scope.Thread)
@Fork(1)
class S {
  implicit val toolbox: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make
  import strymonas.TestPipelines._

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
  def sum_staged () : Int = {
    val res : Int = sumS(v)
    res
  }

  @Benchmark
  def sumOfSquares_staged () : Int = {
    val res : Int = sumOfSquaresS(v)
    res
  }

  @Benchmark
  def sumOfSquaresEven_staged () : Int = {
    val res : Int = sumOfSquaresEvenS(v)
    res
  }

  @Benchmark
  def cart_staged () : Int = {
    val res : Int = cartS(vHi, vLo)
    res
  }

  @Benchmark
  def dotProduct_staged () : Int = {
    val res : Int = dotProductS(vHi, vHi)
    res
  }

  @Benchmark
  def flatMap_after_zip_staged () : Int = {
    val res : Int = flatMap_after_zipS(vFaZ, vFaZ)
    res
  }

  @Benchmark
  def zip_after_flatMap_staged () : Int = {
    val res : Int = zip_after_flatMapS(vZaF, vZaF)
    res
  }

  @Benchmark
  def flatMap_take_staged () : Int = {
    val res : Int = flatMap_takeS(v, vLo)
    res
  }
  
  @Benchmark
  def zip_flat_flat_staged () : Int = {
    val res : Int = zip_flat_flatS(v, vLo)
    res
  }  
}
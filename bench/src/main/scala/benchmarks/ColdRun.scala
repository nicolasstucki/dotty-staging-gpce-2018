package benchmarks

import strymonas.{Stream => SStream}
import scala.quoted._
import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ArrayBuffer

import scala.quoted.Toolbox.Default._

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Measurement(batchSize = 1, iterations = 1, time = 1)
@Warmup(0)
@Fork(10)
class ColdRun {
  import strymonas.TestPipelines._

  @Benchmark
  def sum_staged_init_cold_jvm () : Unit = {
    sum().run
  }

  @Benchmark
  def sumOfSquares_staged_init_cold_jvm () : Unit = {
    sumOfSquares().run
  }

  @Benchmark
  def sumOfSquaresEven_staged_init_cold_jvm () : Unit = {
    sumOfSquaresEven().run
  }

  @Benchmark
  def cart_staged_init_cold_jvm () : Unit = {
    cart().run
  }


  @Benchmark
  def dotProduct_staged_init_cold_jvm () : Unit = {
    dotProduct().run
  }

  @Benchmark
  def flatMap_after_zip_staged_init_cold_jvm () : Unit = {
    flatMap_after_zip().run
  }

  @Benchmark
  def zip_after_flatMap_staged_init_cold_jvm () : Unit = {
    flatMap_take().run
  }

  @Benchmark
  def flatMap_take_staged_init_cold_jvm () : Unit = {
    zip_after_flatMap().run
  }

  @Benchmark
  def zip_flat_flat_staged_init_cold_jvm () : Unit = {
    zip_flat_flat().run
  }
}
package observatory.bearmug

import java.io.PrintWriter
import java.time.LocalDate
import java.util.concurrent.TimeUnit

import observatory.Location
import org.openjdk.jmh.annotations._

import scala.util.Random

@State(Scope.Benchmark)
@Warmup(iterations = 2, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
class ServiceLocatorBenchmark {

  val seqService = ServiceLocator.servePlain()

  val parService = ServiceLocator.serveParallel()

  println(s"build dir is ${System.getProperty("buildDir")}")

  val year = 2015
  val resPath = s"${System.getProperty("buildDir")}/resources/test"
  val stationsFile = s"/jmh/stations-${Random.nextInt()}"
  val temperaturesFile = s"/jmh/temperatures-${Random.nextInt()}"
  val preparedData = ServiceLocator.servePlain().temperaturesOf(year, stationsFile, temperaturesFile)

  @Setup
  def setup(): Unit = {
    val stations = (1 to 10000).map(number => s"$number,$number,${Random.nextDouble()},${Random.nextDouble()}\n")
    new PrintWriter(resPath + stationsFile) {
      stations.foreach(write); close()
    }
    val temperatures = (1 to 10000000).map(_ => {
      val station = Random.nextInt(10000)
      s"$station,$station,${Random.nextInt(11)},${1 + Random.nextInt(28)},${Random.nextInt(100) - 50.0}\n"
    })
    new PrintWriter(resPath + temperaturesFile) {
      temperatures.foreach(write); close()
    }
  }

  @Benchmark
  def plainTemperatures(): Iterable[(LocalDate, Location, Double)] =
    seqService.temperaturesOf(year, stationsFile, temperaturesFile)

  @Benchmark
  def plainAvgTemperature(): Iterable[(Location, Double)] = seqService.yar(preparedData)

  @Benchmark
  def parallelTemperatures(): Iterable[(LocalDate, Location, Double)] =
    parService.temperaturesOf(year, stationsFile, temperaturesFile)

  @Benchmark
  def parallelAvgTemperature(): Iterable[(Location, Double)] = parService.yar(preparedData)
}

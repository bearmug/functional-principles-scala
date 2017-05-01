package observatory.bearmug

import java.io.PrintWriter
import java.time.LocalDate
import java.util.concurrent.TimeUnit

import observatory.Location
import observatory.bearmug.ServiceLocator.Itr
import org.openjdk.jmh.annotations._

import scala.util.Random

@State(Scope.Benchmark)
@Warmup(iterations = 2, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
class ServiceLocatorBenchmark {

  val seqService = ServiceLocator.servePlain()

  val parService = ServiceLocator.serveParallel()

  val year = 2015
  val resPath = s"${System.getProperty("buildDir")}/resources/test"
  private val signature = Random.nextInt(10000000)
  val stationsFile = s"/stations-jmh-$signature"
  val temperaturesFile = s"/temperatures-jmh-$signature"
  var preparedData: Itr = _

  @Setup
  def setup(): Unit = {
    println(s"writing stations data to ${resPath + stationsFile}")
    val stations = (1 to 10000).map(number => s"$number,$number,${Random.nextDouble()},${Random.nextDouble()}\n")
    new PrintWriter(resPath + stationsFile) {
      stations.foreach(write); flush(); close()
    }
    println(s"writing temperature data to ${resPath + temperaturesFile}")
    val temperatures = (1 to 10000000).map(_ => {
      val station = Random.nextInt(10000)
      s"$station,$station,${Random.nextInt(11)},${1 + Random.nextInt(28)},${Random.nextInt(100) - 50.0}\n"
    })
    new PrintWriter(resPath + temperaturesFile) {
      temperatures.foreach(write); flush(); close()
    }

    preparedData = ServiceLocator.servePlain().temperaturesOf(year, stationsFile, temperaturesFile)
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

package observatory.bearmug

import java.time.LocalDate

import observatory.Location
import observatory.bearmug.ServiceLocator.Itr

import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex

sealed abstract class TemperatureService {
  def yar(itr: Itr): scala.Iterable[(Location, Double)]

  def itr(year: Int, stnKey: String, stnSrc: BufferedSource, tmpSrc: BufferedSource): Itr

  final def temperaturesOf(year: Int, stationsFile: String, temperaturesFile: String): Itr =
    itr(
      year,
      stationsFile,
      Source.fromInputStream(getClass.getResourceAsStream(stationsFile)),
      Source.fromInputStream(getClass.getResourceAsStream(temperaturesFile))
    )
}

object Conversions {
  def toCelsius(fahrenheit: Double): Double = (fahrenheit - 32) * 5 / 9

  def toFahrenheit(celsius: Double): Double = (celsius * 9 / 5) + 32
}

object ServiceLocator {

  type Itr = Iterable[(LocalDate, Location, Double)]
  val stnPattern: Regex = "(\\d+),(\\d+),([-|\\+]+\\d+\\.+\\d+),([-|\\+]+\\d+\\.+\\d+)".r
  val tempPattern: Regex = "(\\d+),(\\d+),(\\d+),(\\d+),([+|-]?\\d+\\.?\\d*)".r

  class PlainService extends TemperatureService {

    def stationsData(stnSrc: BufferedSource): Map[String, Location] = stnSrc
      .getLines()
      .flatMap {
        case stnPattern(stn, wban, latitude, longitude) =>
          Some(s"$stn:$wban" -> Location(latitude.toDouble, longitude.toDouble))
        case _ => None
      }.toMap

    override def itr(
                      year: Int,
                      stnKey: String,
                      stnSrc: BufferedSource,
                      tmpSrc: BufferedSource): Itr = {

      val stationsMap = stationsData(stnSrc)

      tmpSrc
        .getLines()
        .flatMap {
          case tempPattern(stn, wban, month, day, tempF) => Some((
            LocalDate.of(year, month.toInt, day.toInt),
            stationsMap(s"$stn:$wban"),
            Conversions.toCelsius(tempF.toDouble)))
          case _ => None
        }.toIterable
    }

    override def yar(itr: Itr): Iterable[(Location, Double)] = itr
      .groupBy(_._2)
      .mapValues(_.foldLeft((0.0, 0)) {
        case ((accTemp, counter), (_, _, temp)) => (accTemp + temp, counter + 1)
      })
      .map {
        case (loc, (totalTemp, totalMeasurements)) => (loc, totalTemp / totalMeasurements)
      }
  }

  def servePlain(): TemperatureService = new PlainService
}

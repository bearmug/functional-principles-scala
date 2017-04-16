package observatory

import observatory.bearmug.ServiceLocator.Itr
import observatory.bearmug.{ServiceLocator, TemperatureService}

/**
  * 1st milestone: data extraction
  */
object Extraction {

  lazy val service: TemperatureService = ServiceLocator.servePlain()

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Itr = {
    service.temperaturesOf(year, stationsFile, temperaturesFile)
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Itr): Iterable[(Location, Double)] = {
    service.yar(records: Itr)
  }
}

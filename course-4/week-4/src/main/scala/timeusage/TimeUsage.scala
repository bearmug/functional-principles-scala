package timeusage

import java.nio.file.Paths

import org.apache.spark.sql._
import org.apache.spark.sql.types._

/**
  * Raw unrounded output
  * +-----------+------+------+------------------+-------------------+------------------+
  * |    working|   sex|   age| avg(primaryNeeds)|          avg(work)|        avg(other)|
  * +-----------+------+------+------------------+-------------------+------------------+
  * |not working|female|active| 12.44098657326328| 0.5045300642148277|10.787291301809692|
  * |not working|female| elder|10.937950058072008| 0.3918408826945412|12.394976771196283|
  * |not working|female| young|12.453092626447285|0.23004265691651435| 11.10018281535649|
  * |not working|  male|active| 11.39641215106732| 0.9282266009852217| 11.37170771756979|
  * |not working|  male| elder|10.725056306306307| 0.6667511261261262|12.326998873873872|
  * |not working|  male| young|11.643116531165312|  0.245420054200542|11.894864498644989|
  * |    working|female|active|11.540577660750273|  4.156430823743584| 8.136794177881796|
  * |    working|female| elder|10.617238578343605| 3.9119803063457335| 9.278544194682052|
  * |    working|female| young| 11.62981898709802|  3.338503755054881| 8.854794916233399|
  * |    working|  male|active| 10.84043561565212|  5.222658175495459| 7.778519700192437|
  * |    working|  male| elder|10.409502336194715| 4.7697308848563855| 8.648572929117314|
  * |    working|  male| young|10.903240058910162|  3.742528227785959|  9.18909671084929|
  * +-----------+------+------+------------------+-------------------+------------------+
  *
  * Rounded statistics
  * +-----------+------+------+------------+----+-----+
  * |    working|   sex|   age|primaryNeeds|work|other|
  * +-----------+------+------+------------+----+-----+
  * |not working|female|active|        12.4| 0.5| 10.8|
  * |not working|female| elder|        10.9| 0.4| 12.4|
  * |not working|female| young|        12.5| 0.2| 11.1|
  * |not working|  male|active|        11.4| 0.9| 11.4|
  * |not working|  male| elder|        10.7| 0.7| 12.3|
  * |not working|  male| young|        11.6| 0.2| 11.9|
  * |    working|female|active|        11.5| 4.2|  8.1|
  * |    working|female| elder|        10.6| 3.9|  9.3|
  * |    working|female| young|        11.6| 3.3|  8.9|
  * |    working|  male|active|        10.8| 5.2|  7.8|
  * |    working|  male| elder|        10.4| 4.8|  8.6|
  * |    working|  male| young|        10.9| 3.7|  9.2|
  * +-----------+------+------+------------+----+-----+
  */
/** Main class */
object TimeUsage {

  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.functions._

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Time Usage")
      .config("spark.master", "local")
      .getOrCreate()

  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits._

  /** Main function */
  def main(args: Array[String]): Unit = {
    timeUsageByLifePeriod()
  }

  def timeUsageByLifePeriod(): Unit = {
    val (columns, initDf) = read("/timeusage/atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)
    val finalDf = timeUsageGrouped(summaryDf)
    finalDf.show()

    val typedDf = timeUsageGroupedTyped(timeUsageSummaryTyped(summaryDf))
    typedDf.show()

    val sqlDf = timeUsageGroupedSql(summaryDf)
    sqlDf.show()
  }

  /** @return The read DataFrame along with its column names. */
  def read(resource: String): (List[String], DataFrame) = {
    val rdd = spark.sparkContext.textFile(fsPath(resource))

    val headerColumns = rdd.first().split(",").to[List]
    // Compute the schema based on the first line of the CSV file
    val schema = dfSchema(headerColumns)

    val data =
      rdd
        .mapPartitionsWithIndex((i, it) => if (i == 0) it.drop(1) else it) // skip the header line
        .map(_.split(",").to[List])
        .map(row)

    val dataFrame =
      spark.createDataFrame(data, schema)

    (headerColumns, dataFrame)
  }

  /** @return The filesystem path of the given resource */
  def fsPath(resource: String): String =
    Paths.get(getClass.getResource(resource).toURI).toString

  /** @return The schema of the DataFrame, assuming that the first given column has type String and all the others
    *         have type Double. None of the fields are nullable.
    * @param columnNames Column names of the DataFrame
    */
  def dfSchema(columnNames: List[String]): StructType = columnNames match {
    case h :: t => StructType(
      StructField(h, StringType, nullable = false) :: t.map(s => StructField(s, DoubleType, nullable = false)))
  }


  /** @return An RDD Row compatible with the schema produced by `dfSchema`
    * @param line Raw fields
    */
  def row(line: List[String]): Row = line match {
    case h :: t => Row.fromSeq(h :: t.map(_.toDouble))
    case _ => throw new IllegalStateException("Unreachable code reached!")
  }

  /** @return The initial data frame columns partitioned in three groups: primary needs (sleeping, eating, etc.),
    *         work and other (leisure activities)
    * @see https://www.kaggle.com/bls/american-time-use-survey
    *
    *      The dataset contains the daily time (in minutes) people spent in various activities. For instance, the column
    *      “t010101” contains the time spent sleeping, the column “t110101” contains the time spent eating and drinking, etc.
    *
    *      This method groups related columns together:
    * 1. “primary needs” activities (sleeping, eating, etc.). These are the columns starting with “t01”, “t03”, “t11”,
    *      “t1801” and “t1803”.
    * 2. working activities. These are the columns starting with “t05” and “t1805”.
    * 3. other activities (leisure). These are the columns starting with “t02”, “t04”, “t06”, “t07”, “t08”, “t09”,
    *      “t10”, “t12”, “t13”, “t14”, “t15”, “t16” and “t18” (those which are not part of the previous groups only).
    */

  val regexPrim = "(t01|t03|t11|t1801|t1803).*"
  val regexWork = "(t05|t1805).*"
  val regexLesr = "(t02|t04|t06|t07|t08|t09|t10|t12|t13|t14|t15|t16|t18).*"

  def classifiedColumns(columnNames: List[String]): (List[Column], List[Column], List[Column]) = (
    columnNames.filter(_.matches(regexPrim)).map(new Column(_)),
    columnNames.filter(_.matches(regexWork)).map(new Column(_)),
    columnNames.filter(s =>
      !s.matches(regexPrim) &&
        !s.matches(regexWork) &&
        s.matches(regexLesr)).map(new Column(_))
  )

  /** @return a projection of the initial DataFrame such that all columns containing hours spent on primary needs
    *         are summed together in a single column (and same for work and leisure). The “teage” column is also
    *         projected to three values: "young", "active", "elder".
    * @param primaryNeedsColumns List of columns containing time spent on “primary needs”
    * @param workColumns         List of columns containing time spent working
    * @param otherColumns        List of columns containing time spent doing other activities
    * @param df                  DataFrame whose schema matches the given column lists
    *
    *                            This methods builds an intermediate DataFrame that sums up all the columns of each group of activity into
    *                            a single column.
    *
    *                            The resulting DataFrame should have the following columns:
    * - working: value computed from the “telfs” column of the given DataFrame:
    *   - "working" if 1 <= telfs < 3
    *   - "not working" otherwise
    * - sex: value computed from the “tesex” column of the given DataFrame:
    *   - "male" if tesex = 1, "female" otherwise
    * - age: value computed from the “teage” column of the given DataFrame:
    *   - "young" if 15 <= teage <= 22,
    *   - "active" if 23 <= teage <= 55,
    *   - "elder" otherwise
    * - primaryNeeds: sum of all the `primaryNeedsColumns`, in hours
    * - work: sum of all the `workColumns`, in hours
    * - other: sum of all the `otherColumns`, in hours
    *
    *                            Finally, the resulting DataFrame should exclude people that are not employable (ie telfs = 5).
    *
    *                            Note that the initial DataFrame contains time in ''minutes''. You have to convert it into ''hours''.
    */
  def timeUsageSummary(
                        primaryNeedsColumns: List[Column],
                        workColumns: List[Column],
                        otherColumns: List[Column],
                        df: DataFrame
                      ): DataFrame = {
    val workingStatusProjection: Column = when($"telfs" >= 1 and $"telfs" < 3, "working").otherwise("not working") as "working"
    val sexProjection: Column = when($"tesex" === 1, "male").otherwise("female") as "sex"
    val ageProjection: Column =
      when($"teage" >= 15 and $"teage" <= 22, "young").
        otherwise(
          when($"teage" >= 23 and $"teage" <= 55, "active").otherwise("elder")
        ) as "age"

    val primaryNeedsProjection: Column = primaryNeedsColumns.reduce((c1, c2) => c1 + c2) / 60 as "primaryNeeds"
    val workProjection: Column = workColumns.reduce((c1, c2) => c1 + c2) / 60 as "work"
    val otherProjection: Column = otherColumns.reduce((c1, c2) => c1 + c2) / 60 as "other"
    df
      .select(workingStatusProjection, sexProjection, ageProjection, primaryNeedsProjection, workProjection, otherProjection)
      .where($"telfs" <= 4) // Discard people who are not in labor force
  }

  /** @return the average daily time (in hours) spent in primary needs, working or leisure, grouped by the different
    *         ages of life (young, active or elder), sex and working status.
    * @param summed DataFrame returned by `timeUsageSumByClass`
    *
    *               The resulting DataFrame should have the following columns:
    * - working: the “working” column of the `summed` DataFrame,
    * - sex: the “sex” column of the `summed` DataFrame,
    * - age: the “age” column of the `summed` DataFrame,
    * - primaryNeeds: the average value of the “primaryNeeds” columns of all the people that have the same working
    *               status, sex and age, rounded with a scale of 1 (using the `round` function),
    * - work: the average value of the “work” columns of all the people that have the same working status, sex
    *               and age, rounded with a scale of 1 (using the `round` function),
    * - other: the average value of the “other” columns all the people that have the same working status, sex and
    *               age, rounded with a scale of 1 (using the `round` function).
    *
    *               Finally, the resulting DataFrame should be sorted by working status, sex and age.
    */
  def timeUsageGrouped(summed: DataFrame): DataFrame = {
    summed
      .groupBy("working", "sex", "age")
      .agg(round(avg("primaryNeeds"), 1) as "primaryNeeds", round(avg("work"), 1) as "work", round(avg("other"), 1) as "other")
      .sort("working", "sex", "age")
  }

  /**
    * @return Same as `timeUsageGrouped`, but using a plain SQL query instead
    * @param summed DataFrame returned by `timeUsageSumByClass`
    */
  def timeUsageGroupedSql(summed: DataFrame): DataFrame = {
    val viewName = s"summed"
    summed.createOrReplaceTempView(viewName)
    spark.sql(timeUsageGroupedSqlQuery(viewName))
  }

  /** @return SQL query equivalent to the transformation implemented in `timeUsageGrouped`
    * @param viewName Name of the SQL view to use
    */
  def timeUsageGroupedSqlQuery(viewName: String): String =
    s"""SELECT
       | working, sex, age, ROUND(AVG(primaryNeeds), 1) as primaryNeeds, ROUND(AVG(work), 1) as work, ROUND(AVG(other), 1) as other
       | FROM $viewName
       | GROUP BY working, sex, age
       | ORDER BY working, sex, age
       | """.stripMargin

  /**
    * @return A `Dataset[TimeUsageRow]` from the “untyped” `DataFrame`
    * @param timeUsageSummaryDf `DataFrame` returned by the `timeUsageSummary` method
    *
    *                           Hint: you should use the `getAs` method of `Row` to look up columns and
    *                           cast them at the same time.
    */
  def timeUsageSummaryTyped(timeUsageSummaryDf: DataFrame): Dataset[TimeUsageRow] =
    timeUsageSummaryDf.map(row => TimeUsageRow(
      row.getAs[String]("working"),
      row.getAs[String]("sex"),
      row.getAs[String]("age"),
      row.getAs[Double]("primaryNeeds"),
      row.getAs[Double]("work"),
      row.getAs[Double]("other")
    ))

  /**
    * @return Same as `timeUsageGrouped`, but using the typed API when possible
    * @param summed Dataset returned by the `timeUsageSummaryTyped` method
    *
    *               Note that, though they have the same type (`Dataset[TimeUsageRow]`), the input
    *               dataset contains one element per respondent, whereas the resulting dataset
    *               contains one element per group (whose time spent on each activity kind has
    *               been aggregated).
    *
    *               Hint: you should use the `groupByKey` and `typed.avg` methods.
    */
  def timeUsageGroupedTyped(summed: Dataset[TimeUsageRow]): Dataset[TimeUsageRow] = {
    summed
      .groupByKey((row: TimeUsageRow) => (row.working, row.sex, row.age))
      .agg(round(avg($"primaryNeeds"), 1).as[Double], round(avg($"work"), 1).as[Double], round(avg($"other"), 1).as[Double])
      .sort($"key._1", $"key._2", $"key._3")
      .map {
        case ((working, sex, age), primaryNeeds, work, other) =>
          TimeUsageRow(working, sex, age, primaryNeeds, work, other)
      }
  }
}

/**
  * Models a row of the summarized data set
  *
  * @param working      Working status (either "working" or "not working")
  * @param sex          Sex (either "male" or "female")
  * @param age          Age (either "young", "active" or "elder")
  * @param primaryNeeds Number of daily hours spent on primary needs
  * @param work         Number of daily hours spent on work
  * @param other        Number of daily hours spent on other activities
  */
case class TimeUsageRow(
                         working: String,
                         sex: String,
                         age: String,
                         primaryNeeds: Double,
                         work: Double,
                         other: Double
                       )
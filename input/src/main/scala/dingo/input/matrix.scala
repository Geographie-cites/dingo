package dingo.input

/*
 * Copyright (C) 2023 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import better.files.*
import scopt.*

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalTime}
import dingo.move.*

import scala.collection.mutable.ListBuffer


@main def create(args: String*) =
  case class Parameter(
    mobilitiesFile: Option[java.io.File] = None,
    resultFile: Option[java.io.File] = None,
    censusFile: Option[java.io.File] = None,
    infectionFile: Option[java.io.File] = None,
    stockFile: Option[java.io.File] = None)

  val builder = OParser.builder[Parameter]

  val parser =
    import builder._
    OParser.sequence(
      programName("dingo-create"),
      opt[java.io.File]("mobilities").required().action((f, p) => p.copy(mobilitiesFile = Some(f))).text("mobilities file").required(),
      opt[java.io.File]("result").required().action((f, p) => p.copy(resultFile = Some(f))).text("result directory"),
      opt[java.io.File]("census").required().action((f, p) => p.copy(censusFile = Some(f))).text("census file for initial population"),
      opt[java.io.File]("infection").required().action((f, p) => p.copy(infectionFile = Some(f))).text("infection cases"),
      opt[java.io.File]("stock").required().action((f, p) => p.copy(stockFile = Some(f))).text("stock file for population evolution")
    )

  case class Index(index: Map[String, Int], reverse: Map[Int, String], all: Seq[String]):
    export index.*

  OParser.parse(parser, args, Parameter()).foreach: parameter =>
    val resultDirectory = parameter.resultFile.get.toScala
    resultDirectory.createDirectories()

    def generateCellIndex(mobilities: File): Index =
      val indexFile = resultDirectory / dingo.data.cellIndex
      indexFile.clear() delete(swallowIOExceptions = true)

      indexFile.appendLine("quadkey,index")
      val quadKeys = collection.mutable.TreeSet[String]()

      for m <- mobilities.lines.drop(1)
      do
        val colums = m.split(",")
        quadKeys.add(colums(0))
        //quadKeys.add(colums(1))

      val keys: Seq[(String, Int)] = quadKeys.toSeq.zipWithIndex
      for (qk, i) <- keys
      do indexFile.appendLine(s"$qk,$i")

      Index(keys.toMap, keys.map(_.swap).toMap, quadKeys.toSeq.sorted)


    def parseCensus(census: File, index: Index) =
      val censusMap =
        census.lines.toSeq.drop(1).map: l =>
          val columns = l.split(",")
            // Check id are sequential

          val population =
            if columns(1) == "NA"
            then 0.0
            else columns(1).toDouble

          (columns(0), population)
        .toMap

      index.all.map(qk => censusMap(qk))

    def generateInitialPopulation(census: File, populationFile: File, index: Index) =
      populationFile.clear() delete (swallowIOExceptions = true)
      populationFile.appendLine("cell,population")

      parseCensus(census, index).zipWithIndex.foreach: (population, qki) =>
        populationFile.append(s"$qki,$population\n")

    def generatePopulationDynamic(census: File, stock: File, populationDynamic: File, index: Index) =
      populationDynamic.clear() delete (swallowIOExceptions = true)

      def parseDate(d: String) =
        val df = DateTimeFormatter.ofPattern("LLL yyyy", java.util.Locale.US)
        val date = df.parse(d)
        val month = date.get(java.time.temporal.ChronoField.MONTH_OF_YEAR)
        val year = date.get(java.time.temporal.ChronoField.YEAR)
        LocalDate.of(year, month, 1).toEpochDay

      var currentId = 0

      val content =
        stock.lines.drop(1).flatMap: l =>
          val columns = l.split(",")
          index.get(columns(0)).map: qki =>
            val date = parseDate(columns(1).filter(_ != '\"'))
            val average = columns(2).toDouble
            (qki, date, average)
        .groupBy((qki, date, _) => (date, qki))


      val allDates = content.keys.map(_._1).toSeq.distinct.sorted

      val censusContent = parseCensus(census, index)

      val initialDate = allDates.head

      val initialPopulationFactor =
        censusContent.zipWithIndex.flatMap: (population, qki) =>
          content.get((initialDate, qki)).map: c =>
            assert(c.size == 1)
            qki -> population / c.head._3
        .toMap

      for
        date <- allDates
      do
        val populations =
          censusContent.zipWithIndex.map: (population, qki) =>
            val stock = content.get((date, qki))
            (stock, initialPopulationFactor.get(qki)) match
              case (Some(s), Some(initialPopulation)) =>
                assert(s.size == 1)
                initialPopulation * s.head._3
                //populationDynamic.appendLine(s"$qki,$date,$adjusted")
              case (None, Some(_)) =>
                scribe.warn(s"no stock data found for date $date and quad key ${index.reverse(qki)}, $population")
                population
                //populationDynamic.appendLine(s"$qki,$date,$population")
              case (_, None) =>
                scribe.warn(s"no initial population data found for quad key ${index.reverse(qki)}, $population")
                //populationDynamic.appendLine(s"$qki,$date,$population")
                population
        assert(populations.size == index.size)
        populationDynamic.appendLine(s"$date,${populations.mkString(",")}")

    def generateMoveListing(mobilities: File, matrixFile: File, index: Index) =
      def parseDashedDate(d: String) =
        val dateElements = d.filterNot(_ == '"').split(" ")
        val df = DateTimeFormatter.ofPattern("yyyy-MM-dd")
        val date = LocalDate.parse(dateElements(0), df).toEpochDay
        val second = LocalTime.parse(dateElements(1)).toSecondOfDay
        (date, second)

      matrixFile.gzipOutputStream().foreach: matrix =>
        val dates =
          mobilities.lines.head.split(",").zipWithIndex.drop(2).map: (d, i) =>
            val (date, time) = parseDashedDate(d)
            (date, time, i)

        def nextQK(f: collection.BufferedIterator[Array[String]]) =
          val qk = f.head(0)
          (qk, dingo.tool.iteratorTakeWhile(f, _(0) == qk).filter(_(2) != "NA"))

        def buildMove(from: String, lines: Seq[Array[String]]): Move =
          val total = lines.map(_(2).toInt).sum
          val byDestination = lines.groupBy(l => index.get(l(1))).view.mapValues(_.map(_(2).toInt).sum.toDouble / total)
          val to = byDestination.toSeq.map((to, r) => Move.To(to, r))
          Move(from = index(from), to = to)

        for
          (date, second, i) <- dates
        do
          val mobilityIterator =
            mobilities.lines.iterator.drop(1).map: l =>
              val c = l.split(",")
              Array(c(0), c(1), c(i))
            .buffered

          val moves = ListBuffer[Move]()

          while mobilityIterator.hasNext
          do
            val (qk, qkData) = nextQK(mobilityIterator)
            moves += buildMove(qk, qkData)

          import io.circe.*
          import io.circe.syntax.*
          matrix.printWriter(true).println(MoveSlice(moves = moves.toSeq, date = date.toInt, second = second).asJson.noSpaces)


    def generateInfection(inputInfectionFile: File, infectionOutputFile: File, index: Index) =
      def parseDashedDate(d: String) =
        val dateElements = d.filterNot(_ == '"').split(" ")
        val df = DateTimeFormatter.ofPattern("yyyy-MM-dd")
        val date = LocalDate.parse(dateElements(0), df).toEpochDay
        val year = LocalDate.parse(dateElements(0), df).getYear
        (date, year)

      val dates =
        inputInfectionFile.lines.head.split(",").zipWithIndex.drop(2).map: (d, i) =>
          val date = parseDashedDate(d)._1
          (date, i)

      for
        (date, i) <- dates
      do
        val qkInfection =
          inputInfectionFile.lines.iterator.drop(1).map: l =>
            val c = l.split(",")
            (c(0), c(i))
          .toMap

        val cases = index.all.map: qk =>
          qkInfection.get(qk) match
            case Some(s) => if s == "NA" then 0 else s.toInt
            case None => 0

        infectionOutputFile.appendLine(s"${date.toInt},${cases.mkString(",")}")


    val index = generateCellIndex(parameter.mobilitiesFile.get.toScala)

    val populationFile = resultDirectory / dingo.data.populationFile
    generateInitialPopulation(parameter.censusFile.get.toScala, populationFile, index)

    val populationDynamic = resultDirectory / dingo.data.populationDynamic
    generatePopulationDynamic(parameter.censusFile.get.toScala, parameter.stockFile.get.toScala, populationDynamic, index)

    val moveMatrixFile = resultDirectory / dingo.data.moveMatrixFile
    generateMoveListing(parameter.mobilitiesFile.get.toScala, moveMatrixFile, index)

    val infectionResult = resultDirectory / dingo.data.infectionFile
    generateInfection(parameter.infectionFile.get.toScala, infectionResult, index)


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
    stockFile: Option[java.io.File] = None)

  val builder = OParser.builder[Parameter]

  val parser =
    import builder._
    OParser.sequence(
      programName("dingo-create"),
      opt[java.io.File]("mobilities").required().action((f, p) => p.copy(mobilitiesFile = Some(f))).text("mobilities file").required(),
      opt[java.io.File]("result").required().action((f, p) => p.copy(resultFile = Some(f))).text("result directory"),
      opt[java.io.File]("census").required().action((f, p) => p.copy(censusFile = Some(f))).text("census file for initial population"),
      opt[java.io.File]("stock").required().action((f, p) => p.copy(stockFile = Some(f))).text("stock file for population evolution")
    )

  case class Index(index: Map[String, Int], reverse: Map[Int, String]):
    export index.*

  OParser.parse(parser, args, Parameter()).foreach: parameter =>
    val resultDirectory = parameter.resultFile.get.toScala
    resultDirectory.createDirectories()

    def generateCellIndex(mobilities: File): Index =
      val indexFile = resultDirectory / "cell-index.csv"
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
      Index(keys.toMap, keys.map(_.swap).toMap)

    def parseCensus(census: File, index: Index) =
      var currentId = 0

      census.lines.toSeq.drop(1).flatMap: l =>
        val columns = l.split(",")
        index.get(columns(0)).map: qki =>
          // Check id are sequential
          assert(qki == currentId)
          currentId += 1

          val population =
            if columns(1) == "NA"
            then 0.0
            else columns(1).toDouble

          (qki, population)

    def generateInitialPopulation(census: File, populationFile: File, index: Index) =
      populationFile.clear() delete (swallowIOExceptions = true)
      populationFile.appendLine("cell,S,E,I,R,population")
      var currentId = 0

      parseCensus(census, index).foreach: (qki, population) =>
        val s = (population * 0.2).round
        val e = (population * 0.2).round
        val i = (population * 0.2).round
        val r = (population * 0.4).round
        populationFile.append(s"$qki,$s,$e,$i,$r,$population\n")

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
        censusContent.flatMap: (qki, population) =>
          content.get((initialDate, qki)).map: c =>
            assert(c.size == 1)
            qki -> population / c.head._3
        .toMap

      for
        date <- allDates
      do
        val populations =
          censusContent.map: (qki, population) =>
            val stock = content.get((date, qki))
            (stock.headOption, initialPopulationFactor.get(qki)) match
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
      matrixFile.gzipOutputStream().map(_.writer).foreach: matrix =>
        def parseDate(d: String) =
          val dateElements = d.filterNot(_ == '"').split(" ")
          val df = DateTimeFormatter.ofPattern("yyyy-MM-dd")
          val date = LocalDate.parse(dateElements(0), df).toEpochDay
          val second = LocalTime.parse(dateElements(1)).toSecondOfDay
          (date, second)

        val dates =
          mobilities.lines.head.split(",").zipWithIndex.drop(2).map: (d, i) =>
            val (date, time) = parseDate(d)
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
          matrix.append(MoveSlice(moves = moves.toSeq, date = date.toInt, second = second).asJson.noSpaces + "\n")

    val index = generateCellIndex(parameter.mobilitiesFile.get.toScala)

    val populationFile = resultDirectory / "population.csv"
    generateInitialPopulation(parameter.censusFile.get.toScala, populationFile, index)

    val populationDynamic = resultDirectory / "population-dynamic.csv"
    generatePopulationDynamic(parameter.censusFile.get.toScala, parameter.stockFile.get.toScala, populationDynamic, index)

    val moveMatrixFile = resultDirectory / "move-matrix.mjson.gz"
    generateMoveListing(parameter.mobilitiesFile.get.toScala, moveMatrixFile, index)

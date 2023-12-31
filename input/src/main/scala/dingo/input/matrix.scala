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

@main def create(args: String*) =
  case class Parameter(
    moveFile: Option[java.io.File] = None,
    resultFile: Option[java.io.File] = None,
    initialPopulation: Option[Int] = None)

  val builder = OParser.builder[Parameter]

  val parser =
    import builder._
    OParser.sequence(
      programName("dingo-create"),
      opt[java.io.File]("move").required().action((f, p) => p.copy(moveFile = Some(f))).text("move file").required(),
      opt[java.io.File]("result").required().action((f, p) => p.copy(resultFile = Some(f))).text("result directory"),
      opt[Int]("initial-population").required().action((f, p) => p.copy(initialPopulation = Some(f))).text("initial population")
    )

  type Index = Map[String, Int]

  OParser.parse(parser, args, Parameter()).foreach: parameter =>
    val resultDirectory = parameter.resultFile.get.toScala
    resultDirectory.createDirectories()

    def generateCellIndex: Index =
      val indexFile = resultDirectory / "cell-index.csv"
      indexFile.clear() delete(swallowIOExceptions = true)

      indexFile.appendLine("quadkey,index")
      val quadKeys = collection.mutable.TreeSet[String]()

      for m <- parameter.moveFile.get.toScala.lines.drop(1)
      do
        val colums = m.split(",")
        quadKeys.add(colums(0))
        //quadKeys.add(colums(1))

      val keys: Seq[(String, Int)] = quadKeys.toSeq.zipWithIndex
      for (qk, i) <- keys
      do indexFile.appendLine(s"$qk,$i")
      keys.toMap

    def generatePopulation(index: Index) =
      val movesValue = parameter.moveFile.get.toScala.lines.drop(1).map(_.split(","))
      val firstDate = movesValue.head(2)
      val firstStep = movesValue.takeWhile(_(2) == firstDate)

      val populationFile = resultDirectory / "population.csv"
      populationFile.delete(swallowIOExceptions = true)

      populationFile.append("cell,size\n")

      val populationData = firstStep.groupBy(_(0)).map((k, v) => index(k) -> v.map(_(3).toInt).sum)
      val scaling = parameter.initialPopulation.get.toDouble / populationData.values.sum

      for i <- index.values.toSeq.sorted
      do
        val pop = populationData.getOrElse(i, 0)
        populationFile.append(s"$i,${pop * scaling}\n")

    def generateMatrix(index: Index) =
      val moveMatrixFile = resultDirectory / "move-matrix.mjson"
      moveMatrixFile.delete(swallowIOExceptions = true)

      def parseMoves(columns: Array[String], totalNumber: Int, index: Index) =
        val dateElements = columns(2).filterNot(_ == '"').split(" ")
        val df = DateTimeFormatter.ofPattern("yyyy-MM-dd")
        val date = LocalDate.parse(dateElements(0), df).toEpochDay
        val time = LocalTime.parse(dateElements(1)).toSecondOfDay
        val number = columns(3).toInt
        Move(from = index(columns(0)), to = index.get(columns(1)), date = date.toInt, second = time, number = number, ratio = number.toDouble / totalNumber)

      def nextStep(i: Iterator[Array[String]]) =
        val firstMove = i.next()
        val firstDate = firstMove(2)
        Seq(firstMove) ++ i.takeWhile(m => m(2) == firstDate)

      val lineIterator = parameter.moveFile.get.toScala.lines.drop(1).iterator.map(_.split(","))

      while
        lineIterator.hasNext
      do
        import io.circe.*
        import io.circe.syntax.*

        val slice = nextStep(lineIterator)
        val total = slice.map(_(3).toInt).sum

        moveMatrixFile.appendLine(slice.map(parseMoves(_, total, index)).asJson.noSpaces)

    val index = generateCellIndex
    generatePopulation(index)
    generateMatrix(index)

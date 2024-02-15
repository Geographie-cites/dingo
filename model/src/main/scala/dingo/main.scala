package dingo

/*
 * Copyright (C) 2024 Romain Reuillon
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

import java.io.{File, Writer}
import better.files.*
import dingo.agent.Human
import dingo.agent.Human.{Serology, serology}
import dingo.move.*

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
//import dingo.stock.{Integration, Stock}
//import dingo.stock.Stock.DynamicEquation
import scopt.*
import scribe.*
import space.*
import io.circe.*
import io.circe.generic.auto.*
import io.circe.yaml

import scala.annotation.tailrec

case class ModelParameters(seed: Long, exposedDuration: Int, infectedDuration: Int, contamination: Double)


@main def model(args: String*) =
  case class Parameter(
    cellIndex: Option[File] = None,
    cellTypology: Option[File] = None,
    population: Option[File] = None,
    populationDynamic: Option[File] = None,
    moveMatrix: Option[File] = None,
    modelParameters: Option[File] = None,
    resultFile: Option[File] = None,
    log: Boolean = false)

  val builder = OParser.builder[Parameter]

  val parser =
    import builder._
    OParser.sequence(
      programName("dingo"),
      opt[File]("cell-index").required().action((f, p) => p.copy(cellIndex = Some(f))).text("cell index file").required(),
      opt[File]("cell-typology").required().action((f, p) => p.copy(cellTypology = Some(f))).text("cell typology file").required(),
      opt[File]("result").action((f, p) => p.copy(resultFile = Some(f))).text("result file"),
      opt[File]("parameters").required().action((f, p) => p.copy(modelParameters = Some(f))).text("parameters file for the model"),
      opt[File]("population").required().action((f, p) => p.copy(population = Some(f))).text("population file"),
      opt[File]("population-dynamic").required().action((f, p) => p.copy(populationDynamic = Some(f))).text("population dynamic file"),
      opt[File]("move-matrix").required().action((f, p) => p.copy(moveMatrix = Some(f))).text("move matrix file"),
      opt[Unit](name = "log").action((f, p) => p.copy(log = true))
    )

  OParser.parse(parser, args, Parameter()).foreach: parameter =>
    if parameter.log
    then scribe.Logger.root.withMinimumLevel(Level.Info).replace()
    else scribe.Logger.root.withMinimumLevel(Level.Warn).replace()

    val modelParameters =
      yaml.parser.parse(parameter.modelParameters.get.toScala.contentAsString).toTry.get.as[ModelParameters].toTry.get

    val random = scala.util.Random(modelParameters.seed)

    run(
      modelParameters = modelParameters,
      cellIndex = parameter.cellIndex.get,
      cellTypology = parameter.cellTypology.get,
      populationFile = parameter.population.get,
      populationDynamicFile = parameter.populationDynamic.get,
      moveMatrixFile = parameter.moveMatrix.get,
      resultFile = parameter.resultFile,
      random = random)


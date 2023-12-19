package dingo

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

import java.io.{File, Writer}
import better.files.*
import dingo.move.Move
import dingo.stock.{Integration, Stock}
import dingo.stock.Stock.DynamicEquation
import scopt.*
import scribe.*
import space.*
import io.circe.*
import io.circe.generic.auto.*
import io.circe.yaml

import scala.annotation.tailrec

case class ModelParameters(beta: Double, gama: Double, infectedRatio: Double, integrationStep: Double)


@main def model(args: String*) =
  case class Parameter(
    cellIndex: Option[File] = None,
    cellTypology: Option[File] = None,
    population: Option[File] = None,
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
      opt[File]("move-matrix").required().action((f, p) => p.copy(moveMatrix = Some(f))).text("move matrix file"),
      opt[Unit](name = "log").action((f, p) => p.copy(log = true))
    )

  OParser.parse(parser, args, Parameter()).foreach: parameter =>
    if parameter.log
    then scribe.Logger.root.withMinimumLevel(Level.Info).replace()
    else scribe.Logger.root.withMinimumLevel(Level.Warn).replace()

    val modelParameters =
      yaml.parser.parse(parameter.modelParameters.get.toScala.contentAsString).toTry.get.as[ModelParameters].toTry.get

    run(
      modelParameters = modelParameters,
      cellIndex = parameter.cellIndex.get,
      cellTypology = parameter.cellTypology.get,
      populationFile = parameter.population.get,
      moveMatrixFile = parameter.moveMatrix.get,
      resultFile = parameter.resultFile)

def run(
  modelParameters: ModelParameters,
  cellIndex: File,
  cellTypology: File,
  populationFile: File,
  moveMatrixFile: File,
  resultFile: Option[File]) =
  val cells: IArray[Cell] = World.readCells(cellIndex.toScala)

  val sir =
    cells.map: c =>
      Stock.sir(modelParameters.beta, modelParameters.gama)

  val stock = Stock.read(populationFile.toScala, modelParameters.infectedRatio)

  def world = World(cells, stock, sir)

  val (firstDay, second) =
    val l = moveMatrixFile.toScala.lines.head
    val m = parser.decode[Array[Move]](l).toTry.get.head
    (m.date, m.second)

  def moves =
    moveMatrixFile.toScala.lines.iterator.map: l =>
      import io.circe.*
      parser.decode[Array[Move]](l).toTry.get

  val resultWriter = resultFile.map(_.toScala.newBufferedWriter)
  try simulation(world, modelParameters, firstDay, moves, resultWriter)
  finally
    resultWriter.foreach(_.close())

def simulation(world: World, modelParameters: ModelParameters, firstDay: Int, moves: Iterator[Array[Move]], resultWriter: Option[Writer]) =
  def time(t: Int) =
    if t % 2 == 0
    then (t / 2 + firstDay, 0)
    else (t / 2 + firstDay, 8 * 3600)

  def simulateDynamic(world: World) =
    def newStocks =
      (world.stocks zip world.dynamic).map: (s, d) =>
        if s.forall(_ == 0.0)
        then s
        else Integration(d).integrate(s, modelParameters.integrationStep, 12.0)

    world.copy(stocks = newStocks)

  def move(moves: Array[Move])(world: World) =
    val newStocks: Array[Array[Double]] =
      Array.tabulate(world.stocks.length, Stock.dimension): (x, y) =>
        world.stocks(x)(y)

    for
      m <- moves
      to <- m.to.toSeq
    do
      val originStock = newStocks(m.from)
      val moving = originStock.map(_ * m.ratio)

      for i <- moving.indices
      do
        newStocks(m.from)(i) =
          val v = originStock(i)
          v - moving(i)

        newStocks(to)(i) =
          val v = newStocks(to)(i)
          v + moving(i)

    world.copy(stocks = IArray.unsafeFromArray(newStocks.map(IArray.unsafeFromArray)))

  def evolve(t: Int, moves: Array[Move]) =
    assert:
      val (d, s) = time(t)
      moves.head.second == s && moves.head.date == d

    simulateDynamic andThen
      move(moves)

  @tailrec def step(world: World, t: Int, moves: Iterator[Array[Move]]): World =
    val (day, sec) = time(t)
    info(s"simulate day $day sec $sec")

    resultWriter.foreach: w =>
      for
        (s, i) <- world.stocks.zipWithIndex
      do w.append(s"$day,$sec,$i,${s.mkString(",")}\n")

    if !moves.hasNext
    then world
    else step(evolve(t, moves.next())(world), t + 1, moves)

  step(world, 0, moves)
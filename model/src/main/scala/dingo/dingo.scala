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
import dingo.agent.Human
import dingo.agent.Human.{Serology, serology}
import dingo.move.Move

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

    val random = scala.util.Random(modelParameters.seed)

    run(
      modelParameters = modelParameters,
      cellIndex = parameter.cellIndex.get,
      cellTypology = parameter.cellTypology.get,
      populationFile = parameter.population.get,
      moveMatrixFile = parameter.moveMatrix.get,
      resultFile = parameter.resultFile,
      random = random)

def run(
  modelParameters: ModelParameters,
  cellIndex: File,
  cellTypology: File,
  populationFile: File,
  moveMatrixFile: File,
  resultFile: Option[File],
  random: Random) =
  val cells: IArray[Cell] = World.readCells(cellIndex.toScala)


  def population = Human.read(populationFile.toScala, modelParameters)

  def world = World(cells, population)

  val (firstDay, second) =
    val l = moveMatrixFile.toScala.lines.head
    val m = parser.decode[Array[Move]](l).toTry.get.head
    (m.date, m.second)

  def moves =
    moveMatrixFile.toScala.lines.iterator.map: l =>
      import io.circe.*
      parser.decode[Array[Move]](l).toTry.get

  val resultWriter = resultFile.map(_.toScala.newBufferedWriter)
  try simulation(world, modelParameters, firstDay, moves, resultWriter, random)
  finally
    resultWriter.foreach(_.close())

def simulation(world: World, modelParameters: ModelParameters, firstDay: Int, moves: Iterator[Array[Move]], resultWriter: Option[Writer], random: Random) =
  def time(t: Int) =
    if t % 2 == 0
    then (t / 2 + firstDay, 0)
    else (t / 2 + firstDay, 8 * 3600)

  def contaminateHuman(world: World) =
    val infected = Array.ofDim[Int](world.cells.length)
    val total = Array.ofDim[Int](world.cells.length)

    world.population.foreach: h =>
      val humanValue = Human.packedIso.reverse.get(h)
      total(humanValue.location) = total(humanValue.location) + 1
      humanValue.serology match
        case Serology.I => infected(humanValue.location) = infected(humanValue.location) + 1
        case _ =>

    val newPopulation =
      world.population.map: h =>
        val humanValue = Human.packedIso.reverse.get(h)
        humanValue.serology match
          case Serology.S =>
            val ratio = infected(humanValue.location).toDouble / total(humanValue.location)
            val newHuman =
              if random.nextDouble() < ratio * modelParameters.contamination then humanValue.copy(serology = Serology.E, update = modelParameters.exposedDuration.toByte)
              else humanValue
            Human.pack(newHuman)
          case _ => h

    world.copy(population = newPopulation)


  def updateSerology(sec: Int)(world: World) =
    def newPopulation =
      world.population.map: h =>
        val update = Human.unpackUpdate(h)
        if update == Serology.noUpdate
        then h
        else
          if update == 0
          then
            Human.packedIso.reverse.modify: h =>
              h.serology match
                case Serology.E => h.copy(serology = Serology.I, update = modelParameters.infectedDuration.toByte)
                case Serology.I => h.copy(serology = Serology.R, update = Serology.noUpdate)
                case s => throw new RuntimeException(s"Serology $s is not supposed to be updated")
            .apply(h)
          else Human.packedIso.reverse.modify(h => h.copy(update = (h.update - 1).toByte))(h)

    if sec == 0
    then world.copy(population = newPopulation)
    else world

  def moveAgents(moves: Array[Move])(world: World): World =
    val indexedPopulation = World.indexPopulation(world)
    val newPopulation = new ArrayBuffer[Human.Packed](world.population.length)
    val indexedMoves =
      moves.groupBy(_.from).mapValues: p =>
        assert(p.size == 1)
        p.head

    for
      cell <- 0 until world.cells.length
    do
      indexedMoves.get(cell) match
        case Some(m) =>
          val cellPopulation = random.shuffle(indexedPopulation(cell))
          val cellSize = cellPopulation.size

          val populationIterator = cellPopulation.iterator
          for to <- random.shuffle(m.to)
          do
            val moving = populationIterator.take(Math.round(to.ratio * cellSize).toInt)
            to.destination match
              case Some(destination) => newPopulation.addAll(moving.map(Human.location.replace(destination.toShort)))
              case None =>
                // For now exiting agents are keep steady
                newPopulation.addAll(moving)

          newPopulation.addAll(populationIterator)

        case None => newPopulation.addAll(indexedPopulation(cell))

    assert(newPopulation.length == world.population.length, s"${newPopulation.length} ${world.population.length}")

    world.copy(population = IArray.unsafeFromArray(newPopulation.toArray))

  def evolve(t: Int, moves: Array[Move]): World => World =
    val (d, s) = time(t)
    assert:
      moves.head.second == s && moves.head.date == d

    updateSerology(s) andThen
      contaminateHuman andThen
      moveAgents(moves)

  @tailrec def step(world: World, t: Int, moves: Iterator[Array[Move]]): World =

    val (day, sec) = time(t)

    if sec == 0
    then
      resultWriter.foreach: w =>
        for
          (c, i) <- World.countByCell(world).zipWithIndex
        do w.append(s"$day,$sec,$i,${c.susceptible},${c.exposed},${c.infected},${c.recovered}\n")

    if !moves.hasNext
    then world
    else
      info(s"simulate day $day sec $sec")
      val newWorld = evolve(t, moves.next())(world)
      step(newWorld, t + 1, moves)

  step(world, 0, moves)
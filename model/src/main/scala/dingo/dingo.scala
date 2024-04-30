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
import dingo.agent.*
import dingo.agent.Human.{Serology, serology}
import dingo.move.*

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scopt.*
import scribe.*
import space.*
import io.circe.*
import io.circe.generic.auto.*
import io.circe.yaml

import scala.annotation.tailrec

enum DayTime(val second: Int):
  case Night extends DayTime(0)
  case Morning extends DayTime(8 * 3600)

def run(
  modelParameters: ModelParameters,
  cellTypology: File,
  dataDirectory: File,
  resultFile: Option[File],
  random: Random) =
  val cells: IArray[Cell] = World.readCells(dataDirectory.toScala / dingo.data.cellIndex)

  assert(modelParameters.serology.length == cells.size, s"${modelParameters.serology.length} == ${cells.size}")
  assert(modelParameters.contamination.length == cells.size, s"${modelParameters.contamination.length} == ${cells.size}")

  def population = Human.read(dataDirectory.toScala / dingo.data.populationFile, modelParameters)
  def world = World(cells, population)

  PopulationDynamic.withPopulationDynamic((dataDirectory.toScala / dingo.data.populationDynamic).toJava): populationDynamic =>
    (dataDirectory.toScala / dingo.data.moveMatrixFile).gzipInputStream().map(_.reader().buffered).map: reader =>
      val moves =
        import scala.jdk.CollectionConverters.*
        reader.lines().iterator().asScala.filter(_.trim.nonEmpty).map: l =>
          import io.circe.*
          parser.decode[MoveSlice](l).toTry.get
        .buffered

      val (moveFirstDay, second) =
        val m = moves.head
        (m.date, m.second)

      scribe.info(s"move first day $moveFirstDay")

      // Skip early population dynamic
      tool.iteratorTakeWhile(populationDynamic, _.date < moveFirstDay)

      val resultWriter = resultFile.map(_.toScala.newBufferedWriter)
      try simulation(world, modelParameters, moveFirstDay, moves, populationDynamic, resultWriter, random)
      finally
        resultWriter.foreach(_.close())

def contaminateHuman(modelParameters: ModelParameters, day: Int, dayTime: DayTime, random: Random, resultWriter: Option[Writer])(world: World) =
  if dayTime == DayTime.Morning
  then
    val infected = Array.ofDim[Int](world.cells.length)
    val total = Array.ofDim[Int](world.cells.length)
    val newInfections = Array.ofDim[Int](world.cells.length)

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
              if random.nextDouble() < ratio * modelParameters.contamination(humanValue.location)
              then
                newInfections(humanValue.location) = newInfections(humanValue.location) + 1
                humanValue.copy(serology = Serology.E, update = modelParameters.exposedDuration.toByte)
              else humanValue
            Human.pack(newHuman)
          case _ => h

    resultWriter.foreach: w =>
      w.append(s"$day,${newInfections.mkString(",")}\n")

    world.copy(population = newPopulation)
  else world

def updateSerology(modelParameters: ModelParameters, dayTime: DayTime)(world: World) =
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

  if dayTime == DayTime.Night
  then world.copy(population = newPopulation)
  else world

def moveAgents(moves: MoveSlice, random: Random)(world: World): World =
  val indexedPopulation = World.indexPopulation(world)
  val newPopulation = new ArrayBuffer[Human.Packed](world.population.length)

  for
    (cell, m) <- (0 until world.cells.length) zip moves.moves
  do
    assert(m.from == cell)
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

  assert(newPopulation.length == world.population.length, s"${newPopulation.length} ${world.population.length}")
  world.copy(population = IArray.unsafeFromArray(newPopulation.toArray))


def updatePopulation(point: PopulationDynamic.PopulationPoint, random: Random)(world: World): World =
  val indexedPopulation = World.indexPopulation(world)
  val newPopulation: Array[Human.Packed] =
    (point._2 zip indexedPopulation).flatMap: (q, population) =>
      val localPopulationSize = population.length
      val diff = q - localPopulationSize
      if diff < 0
      then random.shuffle(population).drop(-diff.toInt)
      else
        val addedIndividuals: Seq[Human.Packed] =
          (0 until diff.toInt).map: _ =>
            if localPopulationSize > 0
            then
              val element = random.nextInt(population.length)
              population(element)
            else
              val element = random.nextInt(world.population.length)
              world.population(element)

        addedIndividuals ++ population

  world.copy(population = IArray.unsafeFromArray(newPopulation))

def simulation(world: World, modelParameters: ModelParameters, moveFirstDay: Int, moves: Iterator[MoveSlice], populationDynamic: PopulationDynamic, resultWriter: Option[Writer], random: Random) =

  @tailrec def step(w1: World, t: Int, moves: collection.BufferedIterator[MoveSlice], populationDynamic: PopulationDynamic): World =
    def time(firstDay: Int, t: Int) =
      if t % 2 == 0
      then (t / 2 + firstDay, DayTime.Night)
      else (t / 2 + firstDay, DayTime.Morning)

    val (day, dayTime) = time(moveFirstDay, t)

    val w2 =
      populationDynamic.headOption match
        case Some(pd) if dayTime == DayTime.Night && pd.date == day =>
          val w = updatePopulation(pd, random)(w1)
          if populationDynamic.hasNext then populationDynamic.next()
          info(s"update population for ${w1.population.length} to ${w.population.length}")
          w
        case _ => w1

    if !moves.hasNext
    then w2
    else
      info(s"simulate day $day $dayTime, total population ${w2.population.length}")
      val move = moves.head
      if move.second == dayTime.second && move.date == day
      then
        def evolve =
          updateSerology(modelParameters, dayTime) andThen
            contaminateHuman(modelParameters, day, dayTime, random, resultWriter) andThen
            moveAgents(move, random)

        val newWorld = evolve(w2)
        moves.next()
        step(newWorld, t + 1, moves, populationDynamic)
      else
        info(s"skipping move in step $day $dayTime: no moves found")
        def evolve =
          updateSerology(modelParameters, dayTime) andThen
            contaminateHuman(modelParameters, day, dayTime, random, resultWriter)

        val newWorld = evolve(w2)
        step(newWorld, t + 1, moves, populationDynamic)

  step(world, 0, moves.buffered, populationDynamic)

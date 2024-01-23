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

import better.files.*
import dingo.agent.*
import dingo.agent.Human.Serology

object space:
  case class Cell(id: Int, quadKey: Long)

  object World:
    def readCells(index: File) =
      val result = collection.mutable.ArrayBuffer[Cell]()
      for
        l <- index.lines.drop(1)
      do
        val c = l.split(",")
        result += Cell(c(1).toInt, c(0).toLong)

      IArray.unsafeFromArray(result.toArray)

    def indexPopulation(world: World) =
      val result = Array.fill(world.cells.size)(new collection.mutable.ArrayBuffer[Human.Packed](1000))

      tool.loop(
        0,
        _ < world.population.length,
        _ + 1
      ): i =>
        val ha = world.population(i)
        val l = Human.unpackLocation(ha)
        result(l) += ha

      result map { _.toArray }

    case class ByCell(susceptible: Int, exposed: Int, infected: Int, recovered: Int)

    def countByCell(world: World) =
      val result = Array.ofDim[Int](world.cells.size, 4)
      for
        ha <- world.population
        h = Human.unpack(ha)
      do
        val current = result(h.location)(h.serology.ordinal)
        result(h.location)(h.serology.ordinal) = current + 1

      result.map: r =>
        ByCell(
          susceptible = r(Serology.S.ordinal),
          exposed = r(Serology.E.ordinal),
          infected = r(Serology.I.ordinal),
          recovered = r(Serology.R.ordinal)
        )

  case class World(cells: IArray[Cell], population: Population)
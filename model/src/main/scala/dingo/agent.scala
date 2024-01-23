package dingo

import java.nio.ByteBuffer


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

import monocle.*
import better.files.*

object agent:
  type Population = IArray[Human.Packed]

  object Human:
    object Serology:
      val noUpdate = -1.toByte
      
    enum Serology:
      case S, E, I, R

    opaque type Packed = IArray[Byte]
    def packedSize = 4

    def pack(h: Human): Packed =
      val buffer = ByteBuffer.allocate(packedSize)
      buffer put low.pack(h.location).unsafeArray
      buffer put low.pack(h.serology.ordinal.toByte).unsafeArray
      buffer put low.pack(h.update).unsafeArray
      val res = buffer.array()
      assert(res.size == packedSize)
      IArray.unsafeFromArray(res)

    def unpack(b: Packed) =
      Human(
        location = unpackLocation(b),
        serology = Human.Serology.fromOrdinal(low.extractByte(b, 2)),
        update = unpackUpdate(b)
      )

    inline def unpackLocation(b: Packed) = low.extractShort(b, 0)
    inline def unpackUpdate(b: Packed) = low.extractByte(b, 3)

    val packedIso = Iso[Human, Packed](pack)(unpack)
    val location = packedIso.reverse andThen Focus[Human](_.location)
    val serology = packedIso.reverse andThen Focus[Human](_.serology)

    def read(populationFile: File, parameters: ModelParameters): Population =
      val population = new collection.mutable.ArrayBuffer[Human.Packed](10000000)

      for
        line <- populationFile.lines.drop(1)
      do
        val colums = line.split(",")
        val l = colums(0).toShort
        val s = colums(1).toInt
        val e = colums(2).toInt
        val i = colums(3).toInt
        val r = colums(4).toInt

        for _ <- 0 until s do population += pack(Human(l, Serology.S, Serology.noUpdate))
        for _ <- 0 until e do population += pack(Human(l, Serology.E, parameters.exposedDuration.toByte))
        for _ <- 0 until i do population += pack(Human(l, Serology.I, parameters.infectedDuration.toByte))
        for _ <- 0 until r do population += pack(Human(l, Serology.R, Serology.noUpdate))

      IArray.unsafeFromArray(population.toArray)


  case class Human(location: Short, serology: Human.Serology, update: Byte)

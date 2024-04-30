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
import bytepack.*

object agent:
  type PopulationDynamic = collection.BufferedIterator[PopulationDynamic.PopulationPoint]

  object PopulationDynamic:
    case class PopulationPoint(date: Long, populations: Array[Double])

    def parse(l: Array[String]) = PopulationPoint(l(0).toLong, l.drop(1).map(_.toDouble))
    def withPopulationDynamic[T](file: java.io.File)(f: PopulationDynamic => T): T =
      import scala.jdk.CollectionConverters.*
      file.toScala.bufferedReader().map: r =>
        val it = r.lines().iterator().asScala.map(l => parse(l.split(","))).buffered
        f(it)
      .get

  type Population = IArray[Human.Packed]

  object Human:
    object Serology:
      val noUpdate = -1.toByte
      
    enum Serology derives EnumMirror:
      case S, E, I, R

    opaque type Packed = IArray[Byte]
    def packedSize = Pack.size[Human]

    def pack(h: Human): Packed = Pack.pack(h)
    def unpack(b: Packed) = Pack.unpack[Human](b)

    inline def unpackLocation(b: Packed) = BytePack.extractShort(b)
    inline def unpackUpdate(b: Packed) = BytePack.extractByte(b, Pack.indexOf[Human](2))

    val packedIso = Iso[Human, Packed](pack)(unpack)
    val location = packedIso.reverse andThen Focus[Human](_.location)
    val serology = packedIso.reverse andThen Focus[Human](_.serology)

    def read(populationFile: File, parameters: ModelParameters): Population =
      val serologyRatio = parameters.serology.map(s => s.map(_ / s.sum))
      val population = new collection.mutable.ArrayBuffer[Human.Packed](10000000)

      for
        line <- populationFile.lines.drop(1)
      do
        val colums = line.split(",")
        val l = colums(0).toShort
        val p  = colums(1).toDouble

        val s = p * serologyRatio(l)(0)
        val e = p * serologyRatio(l)(1)
        val i = p * serologyRatio(l)(2)
        val r = p - s - e - i

        for _ <- 0 until s.toInt do population += pack(Human(l, Serology.S, Serology.noUpdate))
        for _ <- 0 until e.toInt do population += pack(Human(l, Serology.E, parameters.exposedDuration.toByte))
        for _ <- 0 until i.toInt do population += pack(Human(l, Serology.I, parameters.infectedDuration.toByte))
        for _ <- 0 until r.toInt do population += pack(Human(l, Serology.R, Serology.noUpdate))

      IArray.unsafeFromArray(population.toArray)


  case class Human(location: Short, serology: Human.Serology, update: Byte) derives Pack

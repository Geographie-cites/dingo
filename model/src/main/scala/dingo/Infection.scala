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

import better.files.*

object infection:

  // data start day 16754
  def fitness(data: java.io.File, results: Array[java.io.File], dataStartDay: Int): Double =
    val resultStartDay = results.head.toScala.lines.head.split(',').head.toInt
    val resultEndDay = results.head.toScala.lines.last.split(',').head.toInt
    val span = resultEndDay - resultStartDay

    def sumArray(a1: Array[Int], a2: Array[Int]) = (a1 zip a2).map(_ + _)

    def caseInDays(days: Set[Int], f: File): Array[Int] =
      f.lines.map(_.split(',').map(_.toInt)).filter: l =>
        days.contains(l.head)
      .map(_.tail)
      .transpose.map(_.sum).toArray

    val caseData =
      data.toScala.lines.map(_.split(',').map(_.toInt)).filter: l =>
        val d = l.head
        d >= dataStartDay && d < dataStartDay + span - 6
      .map: l =>
        (l.head, l.tail)

    val diffByWeek =
      for
        (d, data) <- caseData
      yield
        val offset = resultStartDay + (d - dataStartDay)
        val allDays = (0 until 7).map(offset + _).toSet

        val resultCases =
          results.map: rf =>
            caseInDays(allDays, rf.toScala)
          .transpose.map(c => c.sum / c.length)

        (data zip resultCases).map((d, r) => Math.abs(d - r))

    val allDiffs = diffByWeek.flatten
    allDiffs.sum.toDouble / allDiffs.size



package dingo

import scala.reflect.ClassTag

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

object tool:
  /** standard C-style for loop */
  inline def loop[A](
    inline start: A,
    inline condition: A => Boolean,
    inline advance: A => A)(inline loopBody: A => Any): Unit =
    var a = start
    while condition(a) do
      loopBody(a)
      a = advance(a)

  def iteratorTakeWhile[T: ClassTag](b: collection.BufferedIterator[T], condition: T => Boolean) =
    val res = collection.mutable.ArrayBuffer[T]()
    while b.hasNext && condition(b.head) do res += b.next
    res.toArray

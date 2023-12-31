package dingo

/*
 * Copyright (C) 2022 Romain Reuillon
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

import java.nio.ByteBuffer

object packing:
  def fillBytes(size: Int, data: IArray[Byte]): IArray[Byte] =
    IArray.fill(size-data.size)(0.toByte) ++ data

  inline def pack(byte: Byte): IArray[Byte] =
    val buff = ByteBuffer.allocate(1)
    buff.put(byte)
    IArray.unsafeFromArray(buff.array())

  inline def pack(short: Short): IArray[Byte] =
    val buff = ByteBuffer.allocate(2)
    buff.putShort(short)
    fillBytes(2, IArray.unsafeFromArray(buff.array()))

  inline def pack(int: Int): IArray[Byte] =
    val buff = ByteBuffer.allocate(4)
    buff.putInt(int)
    fillBytes(4, IArray.unsafeFromArray(buff.array()))

  inline def pack(float: Float): IArray[Byte] =
    val buff = ByteBuffer.allocate(4)
    buff.putFloat(float)
    fillBytes(4, IArray.unsafeFromArray(buff.array()))


  //  inline def unpackByte(b: Iterable[Byte]) = b.head
//  inline def unpackShort(b: Iterable[Byte]) = BigInt(b.take(2).toArray).toShort
//  inline def unpackInt(b: Iterable[Byte]) = BigInt(b.take(4).toArray).toInt

  inline def extractByte(b: IArray[Byte], index: Int = 0) = b(index)
  inline def extractShort(b: IArray[Byte], index: Int = 0) = ByteBuffer.wrap(Array[Byte](b(index), b(index + 1))).getShort
  inline def extractInt(b: IArray[Byte], index: Int = 0) = ByteBuffer.wrap(Array[Byte](b(index), b(index + 1), b(index + 2), b(index + 3))).getInt
  inline def extractFloat(b: IArray[Byte], index: Int = 0) = ByteBuffer.wrap(Array[Byte](b(index), b(index + 1), b(index + 2), b(index + 3))).getFloat


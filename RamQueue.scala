/*

Copyright © 2013 RainWarrior

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

package asmstuff

import collection.IterableLike
import collection.mutable.{ ArrayBuffer, Builder }
import collection.generic.{ GenericTraversableTemplate, Growable, Shrinkable }
import annotation.tailrec

import scalaz._
import Scalaz._

class RamQueue[A](implicit ord: math.Ordering[A]) extends IterableLike[A, ArrayBuffer[A]]
                                                     with GenericTraversableTemplate[A, ArrayBuffer]
                                                     with Growable[A]
                                                     with Shrinkable[A] {
  implicit val or = Order.fromScalaOrdering(ord)
  // data(map(a)) == a
  private[this] val data = ArrayBuffer.empty[A]
  private[this] var map = Map.empty[A, Int]

  private def swap(i: Int, j: Int): Unit = if(i != j) {
    val t = data(i)
    data(i) = data(j)
    data(j) = t
    map += data(i) -> i
    map += data(j) -> j
  }

  @tailrec private def pushUp(pos: Int): Unit = if(pos > 0) {
    val ppos = (pos - 1) / 2
    if(data(ppos) > data(pos)) {
      swap(ppos, pos)
      pushUp(ppos)
    }
  }

  @tailrec private def pushDown(pos: Int): Unit = if(pos * 2 + 1 < data.length) {
    if(data(pos) > data(pos * 2 + 1) &&
       (pos * 2 + 2 >= data.length || data(pos * 2 + 1) <= data(pos * 2 + 2))) { // left is min
      swap(pos, pos * 2 + 1)
      pushDown(pos * 2 + 1)
    } else if(pos * 2 + 2 < data.length && data(pos) > data(pos * 2 + 2)) {
      swap(pos, pos * 2 + 2)
      pushDown(pos * 2 + 2)
    }
  }

  override def seq = data
  override def iterator = data.iterator
  override def companion = ArrayBuffer

  override def +=(a: A): this.type = {
    data += a
    map += a -> (data.length - 1)
    pushUp(data.length - 1)
    this
  }

  override def -=(a: A): this.type = {
    map.get(a) map { pos =>
      swap(pos, data.length - 1)
      data.trimEnd(1)
      map -= a
      if(pos < data.length) {
        if(a < data(pos)) pushDown(pos)
        else if(a > data(pos)) pushUp(pos)
      }
    }
    this
  }

  override def clear(): Unit = {
    data.clear()
    map = Map.empty
  }

}


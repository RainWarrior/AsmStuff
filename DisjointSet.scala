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

import scalaz._
import Scalaz._

class DisjointSet[A] {
  case class ElemData(parent: Option[A], rank: Int)
  var data = Map.empty[A, ElemData]

  def makeSet(a: A): Boolean = {
    if(!data.contains(a)) {
      data += a -> ElemData(None, 0)
      true
    } else false
  }

  def find(a: A): Option[A] = data.get(a) match {
    case None => None
    case Some(ElemData(None, _)) => Some(a)
    case Some(ElemData(Some(parent), r)) =>
      val root = find(parent).get
      data += a -> ElemData(Some(root), r)
      Some(root)
  }

  def union(a: A, b: A): Unit = (for (ar <- find(a); br <- find(b)) yield {
    if(ar != br) {
      val ad = data(ar)
      val bd = data(br)
      if(ad.rank < bd.rank) {
        data += ar -> ElemData(Some(br), ad.rank)
      } else if (ad.rank > bd.rank) {
        data += br -> ElemData(Some(ar), bd.rank)
      } else {
        data ++= Seq(
          ar -> ElemData(Some(br), ad.rank),
          br -> ElemData(bd.parent, bd.rank + 1)
        )
      }
    }
  }) getOrElse {
    throw new IllegalStateException(s"One of the arguments to union isn't in the set: $a, $b")
  }

  def isSame(a: A, b: A) = 
    (for(ar <- find(a); br <- find(b)) yield ar == br) getOrElse false

  def toMap: Map[A, Set[A]] = {
    val pMap = data.keySet.map(k => Map(find(k).get -> Set(k))).sumr
    pMap.valuesIterator.flatMap(s => s.map(a => a -> s)).toMap
  }
}


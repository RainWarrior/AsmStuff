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

import Types._

trait SuperTree {
  def nodes: Set[ClassT]
  def getSuper(t: ClassT): Option[ClassT]
  def getIfaces(t: ClassT): Set[ClassT]

  def getParents(t: ClassT): Seq[ClassT] = getSuper(t).toSeq ++ getIfaces(t)

  lazy val subMap: Map[ClassT, Set[ClassT]] =
    (for(c <- nodes; p <- getParents(c)) yield Map(p -> Set(c))).sumr  
}

trait MethodTree extends SuperTree {
  def getMethods(t: ClassT): Set[MethodAcc]

  private[this] def mergeMethods(c: ClassT, ms: Set[MethodAcc], sms: Set[MethodAcc]): Set[MethodAcc] = {
    ms ++ sms.filter(sm => (sm isInheritedBy c) && !sm.static /*&& ms.forall(m => !(m overrides sm))*/)
  }

  lazy val propagatedMMap: Map[ClassT, Set[MethodAcc]] = {
    val q = new RamQueue[RankedClassMethods]()(math.Ordering.by(_.rank))
    var m = Map.empty[ClassT, RankedClassMethods]
    for(c <- nodes) {
      val rc = RankedClassMethods(c, this)
      q += rc
      m += c -> rc
    }
    while(!q.isEmpty) {
      val head = q.head
      q -= head
      if(head.rank > 0) println(s"Big ref count for ${head.name}: ${head.rank}")
      for(cs <- subMap.get(head.name); c <- cs) {
        val rc = m(c)
        q -= rc
        // copy all inherited methods, update rank
        val nrc = rc.copy(methods = mergeMethods(rc.name, rc.methods, head.methods), rank = rc.rank - 1)
        // update queue
        q += nrc
        m += c -> nrc
      }
    }
    //println(s"propagated methods: ${m.values}")
    m.mapValues(_.methods)
  }

  lazy val fullMMap: Map[MethodS, Set[MethodS]] = {
    val ds = new DisjointSet[MethodS]
    for {
      (c, ms) <- propagatedMMap
      m1 <- ms
      m2 <- ms
      if m1 != m2 && (m1 overrides m2) && !m1.static && !m2.static
    } {
      val m1S = m1.toS
      val m2S = m2.toS
      ds.makeSet(m1S)
      ds.makeSet(m2S)
      ds.union(m1S, m2S)
      //println(s"union: $m1S, $m2S")
    }
    ds.toMap
  }

  def getMethodSet(method: MethodS): Set[MethodS] = fullMMap.get(method).orZero
}

trait MapSuperTree extends SuperTree {
  def sMap: Map[ClassT, ClassT]
  def iMap: Map[ClassT, Set[ClassT]]
  
  override def nodes: Set[ClassT] = iMap.keySet

  override def getSuper(t: ClassT): Option[ClassT] = sMap.get(t)

  override def getIfaces(t: ClassT): Set[ClassT] = iMap.get(t).orZero
}

trait MapMethodTree extends MapSuperTree with MethodTree {
  def mMap: Map[ClassT, Set[MethodAcc]]

  override def getMethods(t: ClassT): Set[MethodAcc] = mMap.get(t).orZero
}


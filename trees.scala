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

import org.objectweb.asm.commons.Remapper

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

trait MethodTree extends SuperTree { self =>
  def getMethods(t: ClassT): Set[MethodAcc]

  def getMethodSet(method: MethodS): Set[MethodS] = fullMMap.get(method).orZero

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

  def translate(mapper: Remapper): MethodTree = new MethodTree {
    private[this] val rMap = (for(n <- nodes) yield Map((mapper map n) -> n)).sumr
    lazy val nodes = self.nodes map mapper.map
    def getSuper(t: ClassT): Option[ClassT] = self getSuper rMap(t) map mapper.map
    def getIfaces(t: ClassT): Set[ClassT] = self getIfaces rMap(t) map mapper.map
    def getMethods(t: ClassT): Set[MethodAcc] = self getMethods rMap(t) map (m => m translate mapper)
  }
}

class MapSuperTree(
    val sMap: Map[ClassT, ClassT],
    val iMap: Map[ClassT, Set[ClassT]]
  ) extends SuperTree {

  override def nodes: Set[ClassT] = iMap.keySet

  override def getSuper(t: ClassT): Option[ClassT] = sMap.get(t)

  override def getIfaces(t: ClassT): Set[ClassT] = iMap.get(t).orZero
}
object MapSuperTree {
  def apply(
      sMap: Map[ClassT, ClassT],
      iMap: Map[ClassT, Set[ClassT]]): MapSuperTree =
    new MapSuperTree(sMap, iMap)

  def apply(t: (
      Map[ClassT, ClassT],
      Map[ClassT, Set[ClassT]]
    )): MapSuperTree = apply(t._1, t._2)
}

class MapMethodTree(
    sMap: Map[ClassT, ClassT],
    iMap: Map[ClassT, Set[ClassT]],
    val mMap: Map[ClassT, Set[MethodAcc]]
  ) extends MapSuperTree(sMap, iMap) with MethodTree { self =>

  override def getMethods(t: ClassT): Set[MethodAcc] = mMap.get(t).orZero

  override def translate(mapper: Remapper): MapMethodTree = new MapMethodTree(
    self.sMap map { case (k, v) => (mapper map k) -> (mapper map v) },
    self.iMap map { case (k, v) => (mapper map k) -> (v map mapper.map) },
    self.mMap map { case (k, v) => (mapper map k) -> (v map (m => m translate mapper)) }
  )
}
object MapMethodTree {
  def apply(
      sMap: Map[ClassT, ClassT],
      iMap: Map[ClassT, Set[ClassT]],
      mMap: Map[ClassT, Set[MethodAcc]]): MapMethodTree =
    new MapMethodTree(sMap, iMap, mMap)

  def apply(t: (
      Map[ClassT, ClassT],
      Map[ClassT, Set[ClassT]],
      Map[ClassT, Set[MethodAcc]]
    )): MapMethodTree = apply(t._1, t._2, t._3)
}

sealed trait TreeInstances0 {
  implicit val mapSuperMonoid = new Monoid[MapSuperTree] {
    def append(t1: MapSuperTree, t2: => MapSuperTree) = new MapSuperTree(
      (t1.sMap ++ t2.sMap),
      (t1.iMap ++ t2.iMap)
    )

    val zero = new MapSuperTree(
      Map.empty[ClassT, ClassT],
      Map.empty[ClassT, Set[ClassT]]
    )
  }
}

trait TreeInstances extends TreeInstances0 {
  implicit val mapMethodMonoid = new Monoid[MapMethodTree] {
    def append(t1: MapMethodTree, t2: => MapMethodTree) = new MapMethodTree(
      (t1.sMap ++ t2.sMap),
      (t1.iMap ++ t2.iMap),
      (t1.mMap ++ t2.mMap)
    )

    val zero = new MapMethodTree(
      Map.empty[ClassT, ClassT],
      Map.empty[ClassT, Set[ClassT]],
      Map.empty[ClassT, Set[MethodAcc]]
    )
  }
}


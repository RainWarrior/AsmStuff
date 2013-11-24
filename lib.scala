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

import java.nio.file.{
  attribute,
  Files,
  FileSystem,
  FileSystems,
  FileVisitResult,
  Path,
  Paths,
  SimpleFileVisitor,
  StandardCopyOption => SCO,
  StandardOpenOption => SOO
}
import attribute.BasicFileAttributes
import java.nio.charset.Charset
import java.net.URI
import java.io.PrintWriter
import java.util.{ List => JList }

import org.objectweb.asm.{
  ClassReader,
  ClassVisitor,
  ClassWriter,
  commons,
  Opcodes,
  tree,
  Type,
  util
}

import commons.{ Remapper, RemappingClassAdapter }
import util.{ ASMifier, Printer, TraceClassVisitor }

import tree.{ AbstractInsnNode, ClassNode, FieldNode, LabelNode, MethodInsnNode, MethodNode }
import Opcodes._

import collection.JavaConversions._
import annotation.tailrec
import language.{ higherKinds, implicitConversions }

import scalaz._
import Scalaz._

import Types._

trait SuperProvider {
  def isProcessed(t: ClassT): Boolean
  def getSuper(t: ClassT): Option[ClassT]
  def getIfaces(t: ClassT): Set[ClassT]
  def getParents(t: ClassT): Seq[ClassT] = getSuper(t).toSeq ++ getIfaces(t)
}

trait MapSuperProvider extends SuperProvider {
  val sMap: Map[ClassT, ClassT]
  val iMap: Map[ClassT, Set[ClassT]]
  
  def isProcessed(t: ClassT): Boolean = iMap.get(t).isEmpty

  override def getSuper(t: ClassT): Option[ClassT] = sMap.get(t)

  override def getIfaces(t: ClassT): Set[ClassT] = iMap.get(t).orZero
}

trait OverrideProvider extends SuperProvider {
  def getMethods(t: ClassT): Set[MethodAcc]
  def getMethodSet(method: MethodS): Set[MethodS]
}

trait MapOverrideProvider extends MapSuperProvider with OverrideProvider {
  val mMap: Map[ClassT, Set[MethodAcc]]

  def mergeMethods(c: ClassT, ms: Set[MethodAcc], sms: Set[MethodAcc]): Set[MethodAcc] = {
    ms ++ sms.filter(sm => (sm isInheritedBy c) && !sm.static /*&& ms.forall(m => !(m overrides sm))*/)
  }

  lazy val subMap: Map[ClassT, Set[ClassT]] =
    (for(c <- iMap.keySet; p <- getParents(c)) yield Map(p -> Set(c))).sumr
    
  lazy val propagatedMMap: Map[ClassT, Set[MethodAcc]] = {
    val q = new RamQueue[RankedClassMethods]()(math.Ordering.by(_.rank))
    var m = Map.empty[ClassT, RankedClassMethods]
    for(c <- mMap.keysIterator) {
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

  override def getMethods(t: ClassT): Set[MethodAcc] = mMap.get(t).orZero
  override def getMethodSet(method: MethodS): Set[MethodS] = fullMMap.get(method).orZero
}

trait Unmapper extends Remapper {
  def addField(pair: (FieldU, FieldName)): Unit
  def addMethod(pair: (MethodU, MethodName)): Unit
  def addClass(pair: (ClassT, ClassT)): Unit
  def mapField(tpe: ClassT, name: FieldName, desc: FieldDesc): Option[FieldName]
  def mapMethod(tpe: ClassT, name: FieldName, ret: ClassT, args: Seq[ClassT]): Option[MethodName]
  def mapClass(tpe: ClassT): Option[String]

  override def mapFieldName(tpe: String, name: String, desc: String): String = 
    mapField(tpe, name, desc).getOrElse(name)
  override def mapMethodName(tpe: String, name: String, desc: String): String =
    mapMethod(
      tpe,
      name,
      Type.getReturnType(desc).getDescriptor,
      Type.getArgumentTypes(desc).map(_.getDescriptor)
    ).getOrElse(name)
  override def map(tpe: String): String =
    mapClass(tpe).getOrElse(tpe)
}

trait MethodPropagatingUnmapper extends Unmapper {
  val prov: OverrideProvider

  abstract override def addMethod(pair: (MethodU, MethodName)): Unit = {
    super.addMethod(pair)
    val (mU, newMS) = pair
    val mS = mU.toS
    for (m <- prov.getMethodSet(mS)) {
      //println(s"method checking: $mS, $m")
      val mU2 = m.toU
      val newM = mapMethod(mU2.owner, mU2.name, mU2.ret, mU2.args)
      //println(newM)
      if(!newM.empty && newMS != newM.get) {
        //throw new IllegalStateException(s"Mapping conflict: ($mS -> $newMS) and ($m -> ${newM.get})")
        println(s"Mapping conflict: ($mS -> $newMS) and ($m -> ${newM.get})")
      } else {
        //super.addMethod((m.toU, newMS))
      }
    }
  }
}

trait SimpleUnmapper extends Unmapper {
  @inline private def t(s: String): Type = Type.getType(s)

  var mappedFields = Map.empty[FieldU, FieldName]
  var mappedMethods = Map.empty[MethodU, MethodName]
  var mappedClasses = Map.empty[ClassT, ClassT]

  /*for (s <- List("D"))
    addClass((s, s))*/

  override def mapField(tpe: String, name: String, desc: String): Option[String] =
    mappedFields.get(FieldU(tpe, name, desc))
  override def mapMethod(tpe: String, name: String, ret: String, args: Seq[String]): Option[String] =
    mappedMethods.get(MethodU(tpe, name, ret, args))
  override def mapClass(tpe: String): Option[String] =
    mappedClasses.get(tpe).orElse(if(Type.getType(tpe).getSort != Type.OBJECT) Some(tpe) else None)

  override def addField(pair: (FieldU, FieldName)) = mappedFields.get(pair._1) match {
    case Some(t) => if(t != pair._2)
      throw new IllegalArgumentException(s"Already have $t while setting ${pair._1} to ${pair._2}")
    case None =>
      mappedFields += pair
  }
  override def addMethod(pair: (MethodU, MethodName)) = mappedMethods.get(pair._1) match {
    case Some(t) => if(t != pair._2)
      throw new IllegalArgumentException(s"Already have $t while setting ${pair._1} to ${pair._2}")
    case None =>
      mappedMethods += pair
  }
  override def addClass(pair: (ClassT, ClassT)) = mappedClasses.get(pair._1) match {
    case Some(t) => if(t != pair._2)
      throw new IllegalArgumentException(s"Already have $t while setting ${pair._1} to ${pair._2}")
    case None =>
      mappedClasses += pair
  }

  def toSrg: Seq[String] = { // Incomplete
    for(t <- mappedClasses.toSeq.sorted) yield s"CL: ${t._1} ${t._2}"
  }
}

trait MapSuperCommon extends ClassWriter {
  val prov: SuperProvider
  
  override def getCommonSuperClass(t1: ClassT, t2: ClassT): ClassT = {
    @tailrec def rec(t1: ClassT, t2: ClassT): ClassT = {
      if(isAssignableFrom(t1, t2)) t1
      else if(isAssignableFrom(t2, t1)) t2
      else prov.getSuper(t1) match {
        case Some(t3) => rec(t3, t2)
        case _ => "java/lang/Object"
      }
    }
    rec(t1, t2)
  }

  def isAssignableFrom(t1: ClassT, t2: ClassT): Boolean = {
    if(t1 == t2) true
    else if(t2 == "java/lang/Object") false
    else prov.getParents(t2).exists(t3 => isAssignableFrom(t1, t3))
  }
}

class MapWriter(val prov: SuperProvider, flags: Int) extends ClassWriter(flags) with MapSuperCommon


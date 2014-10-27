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

import org.objectweb.asm.{ commons, Type }
import commons.Remapper

import scalaz._
import Scalaz._

import scala.math.Ordering

import Types._

trait Unmapper extends Remapper {
  def addField(pair: (FieldU, FieldName)): Unit
  def addMethod(pair: (MethodU, MethodName)): Unit
  def addClass(pair: (ClassT, ClassT)): Unit
  def mapField(tpe: ClassT, name: FieldName, desc: FieldDesc): Option[FieldName]
  def mapMethod(tpe: ClassT, name: MethodName, ret: FieldDesc, args: Seq[FieldDesc]): Option[MethodName]
  def mapClass(tpe: ClassT): Option[ClassT]

  override def mapFieldName(tpe: String, name: String, desc: String): String = 
    mapField(tpe, name, "").getOrElse(name) // no field types in SRG, workaround
  override def mapMethodName(tpe: String, name: String, desc: String): String =
    mapMethod(
      tpe,
      name,
      Type.getReturnType(desc).getDescriptor,
      Type.getArgumentTypes(desc).map(_.getDescriptor)
    ).getOrElse(name)
  override def map(tpe: String): String =
    mapClass(tpe).getOrElse(tpe)

  def mapFieldDesc(desc: FieldDesc): Option[FieldDesc] = {
    val t = Type.getType(desc)
    t.getSort match {
      case Type.ARRAY =>
        mapFieldDesc(t.getElementType.getDescriptor) map ("[" * t.getDimensions + _)
      case Type.OBJECT =>
        mapClass(t.getInternalName) map (n => Type.getObjectType(n).getDescriptor)
    }
  }
}

trait MethodPropagatingUnmapper extends Unmapper {
  val tree: MethodTree

  abstract override def addMethod(pair: (MethodU, MethodName)): Unit = {
    super.addMethod(pair)
    val (mU, newMS) = pair
    val mS = mU.toS
    for (m <- tree.getMethodSet(mS)) {
      //println(s"method checking: $mS, $m")
      val mU2 = m.toU
      val newM = mapMethod(mU2.owner, mU2.name, mU2.ret, mU2.args)
      //println(newM)
      if(!newM.isEmpty && newMS != newM.get) {
        //throw new IllegalStateException(s"Mapping conflict: ($mS -> $newMS) and ($m -> ${newM.get})")
        println(s"Mapping conflict: ($mS -> $newMS) and ($m -> ${newM.get})")
      } else {
        super.addMethod((m.toU, newMS))
      }
    }
  }
}

trait SimpleUnmapper extends Unmapper {
  @inline private def t(s: String): Type = Type.getType(s)

  val stree: SuperTree

  var mappedFields = Map.empty[FieldU, FieldName]
  var mappedMethods = Map.empty[MethodU, MethodName]
  var mappedClasses = Map.empty[ClassT, ClassT]

  /*for (s <- List("D"))
    addClass((s, s))*/

  override def mapField(tpe: ClassT, name: FieldName, desc: FieldDesc): Option[String] =
    mappedFields.get(FieldU(tpe, name, desc)) orElse {
      stree.getIfaces(tpe).flatMap(i => mapField(i, name, desc)).headOption
    } orElse {
      stree.getSuper(tpe).flatMap(s => mapField(s, name, desc))
    }
    //mappedFields.get(FieldU(tpe, name, desc))
  override def mapMethod(tpe: ClassT, name: MethodName, ret: FieldDesc, args: Seq[FieldDesc]): Option[String] =
    // Not entirely correct, but should suffice
    mappedMethods.get(MethodU(tpe, name, ret, args)) orElse {
      stree.getSuper(tpe).flatMap(s => mapMethod(s, name, ret, args))
    } orElse {
      stree.getIfaces(tpe).flatMap(i => mapMethod(i, name, ret, args)).headOption
    }
  override def mapClass(tpe: ClassT): Option[ClassT] =
    mappedClasses.get(tpe)//.orElse(if(Type.getType(tpe).getSort != Type.OBJECT) Some(tpe) else None) // FIXME

  override def addField(pair: (FieldU, FieldName)) = mappedFields.get(pair._1) match {
    case Some(t) => if(t != pair._2)
      //throw new IllegalArgumentException(s"Already have $t while setting ${pair._1} to ${pair._2}")
      println(s"Already have $t while setting ${pair._1} to ${pair._2}")
    case None =>
      mappedFields += pair
  }
  override def addMethod(pair: (MethodU, MethodName)) = mappedMethods.get(pair._1) match {
    case Some(t) => if(t != pair._2)
      //throw new IllegalArgumentException(s"Already have $t while setting ${pair._1} to ${pair._2}")
      println(s"Already have $t while setting ${pair._1} to ${pair._2}")
    case None =>
      mappedMethods += pair
  }
  override def addClass(pair: (ClassT, ClassT)) = mappedClasses.get(pair._1) match {
    case Some(t) => if(t != pair._2)
      //throw new IllegalArgumentException(s"Already have $t while setting ${pair._1} to ${pair._2}")
      println(s"Already have $t while setting ${pair._1} to ${pair._2}")
    case None =>
      mappedClasses += pair
  }

  def toSrg: Vector[String] = {
    implicit val fieldOrdering = Ordering.by((_: FieldU).toString)
    implicit val methodOrdering = Ordering.by((_: MethodU).toString)

    (for(t <- mappedClasses.toVector.sorted; (k, v) = t) yield s"CL: $k $v") ++
    (for(t <- mappedFields.toVector.sorted; (k, v) = t) yield s"FD: $k ${k translate this}") ++
    (for(t <- mappedMethods.toVector.sorted; (k, v) = t) yield s"MD: $k ${k translate this}")
  }
}

trait ChainUnmapper extends Unmapper {
  val first: Unmapper
  val second: Unmapper

  def addField(pair: (FieldU, FieldName)): Unit = ???
  def addMethod(pair: (MethodU, MethodName)): Unit = ???
  def addClass(pair: (ClassT, ClassT)): Unit = ???

  def mapField(tpe: ClassT, name: FieldName, desc: FieldDesc): Option[FieldName] = for {
    tpe1 <- first.mapClass(tpe)
    name1 <- first.mapField(tpe, name, desc)
    desc1 <- first.mapFieldDesc(desc)
    name2 <- second.mapField(tpe1, name1, desc1)
  } yield name2

  def mapMethod(tpe: ClassT, name: MethodName, ret: FieldDesc, args: Seq[FieldDesc]): Option[MethodName] = for {
    tpe1 <- first.mapClass(tpe)
    name1 <- first.mapMethod(tpe, name, ret, args)
    ret1 <- first.mapFieldDesc(ret)
    args1 <- args.to[Vector].traverse(first.mapFieldDesc)
    name2 <- second.mapMethod(tpe1, name1, ret1, args1)
  } yield name2

  def mapClass(tpe: ClassT): Option[ClassT] = for {
    tpe1 <- first.mapClass(tpe)
    tpe2 <- second.mapClass(tpe1)
  } yield tpe2
}

trait UnmapperFunctions {
  def MethodPropagatingUnmapper(t: MethodTree) = new SimpleUnmapper with MethodPropagatingUnmapper {
    val tree = t
    val stree = t
  }
  def ChainUnmapper(f: Unmapper, s: Unmapper) = new ChainUnmapper {
    val first = f
    val second = s
  }
}

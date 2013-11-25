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

import Types._

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
  val tree: MethodTree

  lazy val destTree = tree translate this

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

  def toSrg: Seq[String] = {
    implicit val fieldOrdering = Ordering.by((_: FieldU).toString)
    implicit val methodOrdering = Ordering.by((_: MethodU).toString)

    (for(t <- mappedClasses.toSeq.sorted; (k, v) = t) yield s"CL: $k $v") ++
    (for(t <- mappedFields.toSeq.sorted; (k, v) = t) yield s"FD: $k ${k translate this}") ++
    (for(t <- mappedMethods.toSeq.sorted; (k, v) = t) yield s"MD: $k ${k translate this}")
  }
}


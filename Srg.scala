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

import java.nio.file.{ Files, Path }
import java.nio.charset.Charset

import org.objectweb.asm.Type

import collection.JavaConversions._
import language.higherKinds

import scalaz._
import Scalaz._

import Types._

object Srg {
  object Descriptor {
    def unapply(s: String): Option[(Seq[String], String)] = {
      try {
        Some((Type.getArgumentTypes(s).map(_.getDescriptor), Type.getReturnType(s).getDescriptor))
      } catch {
        case e: Exception => None
      }
    }
  }

  def spaceR(n: Int) = Seq.fill(n)("([^ #]*)").mkString(" ").+(" ?").r
  val Line = """^(..): ((?:[^ #]* ?)*)(#.*)?$""".r
  val Str2 = spaceR(2)
  val Str4 = spaceR(4)
  val Pkg = """(.*)/([^/]*)""".r

  def fromPath(p: Path, um: Unmapper) =
    load(Files.readAllLines(p, Charset.defaultCharset).to[Vector], um)

  def fromPathReverse(p: Path, um: Unmapper) =
    load(Files.readAllLines(p, Charset.defaultCharset).to[Vector] map revLine, um)

  def load[F[_]: Functor](lines: F[String], um: Unmapper): Unit = {
    for(l <- lines) yield l match {
      case Line("PK", Str2(oldpkg, newpkg), comment) =>
      case Line("CL", Str2(oldCl, newCl), comment) =>
        um.addClass(oldCl -> newCl)
      case Line("FD", Str2(Pkg(oldCl, oldFd), Pkg(newCl, newFd)), comment) =>
        um.addClass(oldCl -> newCl)
        um.addField(FieldU(oldCl, oldFd, "") -> newFd) // No field type in srg
      case Line("MD", Str4(
          Pkg(oldCl, oldMd), Descriptor(oldArgs, oldRt), 
          Pkg(newCl, newMd), Descriptor(newArgs, newRt)), comment)
          if oldArgs.length == newArgs.length =>
        um.addClass(oldCl -> newCl)
        for {
          (oldArg, newArg) <- (oldArgs, newArgs).zipped.view
          oat <- fieldToInternal(oldArg)
          nat <- fieldToInternal(newArg)
        } um.addClass(oat -> nat)
        for {
          ort <- fieldToInternal(oldRt)
          nrt <- fieldToInternal(newRt)
        } um.addClass(ort -> nrt)
        um.addMethod(MethodU(oldCl, oldMd, oldRt, oldArgs) -> newMd)
        //if(oldCl == "fz") println(s"Method: $oldCl/$oldMd:${oldArgs.mkString} -> $newMd")
        //println(s"Method: $oldCl/$oldMd:${oldArgs.mkString} -> $newMd")
      case _ => throw new IllegalArgumentException(s"""Wrong line in SRG file: "$l" """)
    }
  }

  def revLine(line: String) = line match {
      case Line(tp, Str2(a, b), comment) =>
        s"$tp: $b $a ${Option(comment).orZero}"
      case Line(tp, Str4(a1, a2, b1, b2), comment) =>
        s"$tp: $b1 $b2 $a1 $a2 ${Option(comment).orZero}"
  }

  def reverse[F[_]: Functor](lines: F[String]): F[String] = lines map revLine
}


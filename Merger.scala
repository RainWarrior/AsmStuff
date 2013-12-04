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

import java.nio.file.{ Files, Path, Paths }
import java.nio.charset.Charset
import java.util.ArrayList

import org.objectweb.asm.{ ClassVisitor, ClassWriter, FieldVisitor, MethodVisitor, Opcodes, Type, tree }
import tree.{ AnnotationNode, ClassNode, FieldNode, MethodNode }

import collection.JavaConversions._
import language.higherKinds

import scalaz._
import Scalaz._

import Util._

object Merger {

  val sideDesc = "Lcpw/mods/fml/relauncher/Side;"
  val sideOnlyDesc = "Lcpw/mods/fml/relauncher/SideOnly;"

  def getAnnNode(side: String) = {
    val node = new AnnotationNode(sideOnlyDesc)
    node.visitEnum("value", sideDesc, side)
    node
  }

  class AnnotationClassAdder(cv: ClassVisitor, val side: String) extends ClassVisitor(Opcodes.ASM4, cv) {
    var done = false

    @inline private[this] def tryAdd(): Unit  = if(!done) {
      val av = cv.visitAnnotation(sideOnlyDesc, true)
      Option(av) map (_.visitEnum("value", sideDesc, side))
      done = true
    }

    override def visitInnerClass(name: String, outer: String, inner: String, access: Int): Unit = {
      tryAdd()
      super.visitInnerClass(name, outer, inner, access)
    }

    override def visitField(access: Int, name: String, desc: String, signature: String, value: AnyRef): FieldVisitor = {
      tryAdd()
      super.visitField(access, name, desc, signature, value)
    }

    override def visitMethod(access: Int, name: String, desc: String, signature: String, exs: Array[String]): MethodVisitor = {
      tryAdd()
      super.visitMethod(access, name, desc, signature, exs)
    }
  }

  @inline final def annotateField(f: FieldNode, side: String) = {
    if(f.visibleAnnotations == null) f.visibleAnnotations = new ArrayList[AnnotationNode]
    f.visibleAnnotations add getAnnNode(side)
  }

  @inline final def annotateMethod(m: MethodNode, side: String) = {
    if(m.visibleAnnotations == null) m.visibleAnnotations = new ArrayList[AnnotationNode]
    m.visibleAnnotations add getAnnNode(side)
  }

  def mergeJars(client: Path, server: Path, out: Path) {
    val clientFiles = Walk(client).toSet.map(client relativize _)
    val serverFiles = Walk(server).toSet.map(server relativize _)

    // copy resources
    for {
      (in, fs) <- Seq(
        client -> clientFiles,
        server -> serverFiles
      )
      f <- fs
      if !(f.toString endsWith ".class")
    } {
      copyFile(in resolve f, out resolve f)
    }

    val Seq(clientClasses, serverClasses) = Seq(clientFiles, serverFiles) map (_ map (_.toString) filter (_ endsWith ".class"))

    // copy unique classes
    for {
      (in, side, fs) <- Seq(
        (client, "CLIENT", clientClasses diff serverClasses),
        (server, "SERVER", serverClasses diff clientClasses)
      )
      f <- fs
    } {
      writeBytes(
        transformClass(readClass(in resolve f))()(new AnnotationClassAdder(_, side)),
        out resolve f
      )
    }

    // process rest of classes
    for(f <- clientClasses intersect serverClasses) {
      val Seq(clientNode, serverNode) = Seq(client, server) map { in =>
        runProvider(readClass(in resolve f), new ClassNode)
      }

      var changed = false

      // fields
      val Seq(clientFMap, serverFMap) = Seq(clientNode, serverNode) map (_.fields.map(f => f.name -> f).toMap)
      val clientFields = clientFMap.keySet
      val serverFields = serverFMap.keySet

      // mark unique fields and copy to client node
      for(fk <- clientFields diff serverFields; f = clientFMap(fk)) {
        annotateField(f, "CLIENT")
      }
      if(clientNode.fields == null) clientNode.fields = new ArrayList[FieldNode]
      for(fk <- serverFields diff clientFields; f = serverFMap(fk)) {
        annotateField(f, "SERVER")
        clientNode.fields add f
        changed = true
      }

      // methods
      val Seq(clientMMap, serverMMap) = Seq(clientNode, serverNode) map (_.methods.map(f => f.name -> f).toMap)
      val clientMethods = clientMMap.keySet
      val serverMethods = serverMMap.keySet

      // mark unique methods and copy to client node
      for(mk <- clientMethods diff serverMethods; m = clientMMap(mk)) {
        annotateMethod(m, "CLIENT")
      }
      if(clientNode.methods == null) clientNode.methods = new ArrayList[MethodNode]
      for(mk <- serverMethods diff clientMethods; m = serverMMap(mk)) {
        annotateMethod(m, "SERVER")
        clientNode.methods add m
        changed = true
      }

      if(changed) writeBytes(
        runProvider(clientNode, new ClassWriter(ClassWriter.COMPUTE_MAXS)).toByteArray,
        out resolve f
      ) else {
        copyFile(client resolve f, out resolve f)
      }
    }
  }
  def main(args: Array[String]) {
    val clientZip = openZip(Paths.get(args(0)))
    val serverZip = openZip(Paths.get(args(1)))
    val outZip = openZip(Paths.get(args(2)), true)
    mergeJars(clientZip.getPath("/"), serverZip.getPath("/"), outZip.getPath("/"))
    clientZip.close()
    serverZip.close()
    outZip.close()
  }
}


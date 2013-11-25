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

object Types {
  type ClassT = String

  type MethodName = String
  type MethodDesc = String

  case class MethodS(owner: ClassT, name: MethodName, desc: MethodDesc) {
    def toU = {
      val dType = Type.getType(desc)
      MethodU(owner, name, dType.getReturnType.getDescriptor, dType.getArgumentTypes.map(_.getDescriptor))
    }
    override def toString = s"($owner/$name $desc)"
  }
  case class MethodAcc(owner: ClassT, name: MethodName, desc: MethodDesc, access: Int) {
    def toS = MethodS(owner, name, desc)

    def overrides(sm: MethodAcc) = {
      name == sm.name &&
      desc == sm.desc &&
      (sm isInheritedBy owner) && {
        if(this.static && !sm.static) println("Static hiding non-static: $this, $sm")
        true
      }
    }

    def static = (access & ACC_STATIC) != 0

    def isInheritedBy(c: ClassT) = {
      owner == c ||
      ((access & (ACC_PUBLIC | ACC_PROTECTED)) != 0) ||
      ((access & (ACC_PUBLIC | ACC_PROTECTED | ACC_PRIVATE)) == 0 && pkg(owner) == pkg(c)) 
    }

    def translate(mapper: Remapper) = MethodAcc(
      mapper map owner,
      mapper mapMethodName (owner, name, desc),
      mapper mapMethodDesc desc,
      access
    )
  }
  case class MethodU(owner: ClassT, name: MethodName, ret: ClassT, args: Seq[ClassT]) {
    def toS = MethodS(owner, name, Type.getMethodDescriptor(Type.getType(ret), args.map(Type.getType): _*))

    def translate(mapper: Remapper) = MethodU(
      mapper map owner,
      mapper mapMethodName (owner, name, toS.desc),
      mapper mapDesc ret ,
      args map mapper.mapDesc
    )

    override def toString = s"$owner/$name ${toS.desc}"
  }

  type FieldName = String
  type FieldDesc = String
  case class FieldU(owner: ClassT, name: FieldName, desc: FieldDesc) {

    def translate(mapper: Remapper) = FieldU(
      mapper map owner,
      mapper mapFieldName (owner, name, desc),
      if(desc == "") "" else mapper mapDesc desc
    )

    override def toString = s"$owner/$name"
  }

  def pkg(cls: ClassT) = {
    val sl = cls.lastIndexOf('/')
    val slFix = if(sl == -1) 0 else sl
    cls.substring(0, slFix)
  }

  case class RankedClassMethods(name: ClassT, methods: Set[MethodAcc], rank: Int)

  object RankedClassMethods {
    def apply(name: ClassT, tree: MethodTree) =
      new RankedClassMethods(
        name,
        tree getMethods name,
        (tree getParents name filter tree.nodes).size
      )
  }
}

import Types._

trait ClassProvider[T] {
  def point(p: T)(v: ClassVisitor): ClassVisitor
}

trait ClassProviderInstances {
  def classProvider[T](f: T => ClassVisitor => ClassVisitor) = new ClassProvider[T] {
    def point(p: T)(v: ClassVisitor): ClassVisitor = f(p)(v)
  }
  implicit val readerProvider = classProvider[ClassReader] { r => v => { r.accept(v, ClassReader.EXPAND_FRAMES); v } }
  implicit val nodeProvider = classProvider[ClassNode] { n => v => { n.accept(v); v } }
}

object Util extends ClassProviderInstances with TreeInstances with UnmapperFunctions {
  //implicit def crToFunction[V <: ClassVisitor](cr: ClassReader) = (v: V) => cr.accept(v, 0) // CV => Unit
  implicit def rvToFunction(r: Remapper) = (v: ClassVisitor) => new RemappingClassAdapter(v, r) // CV => CV
  implicit def wToFunction[P: ClassProvider](cw: ClassWriter) = (p: P) => implicitly[ClassProvider[P]].point(p)(cw)
  implicit def printerToFunction(t: (Printer, PrintWriter)) = (v: ClassVisitor) => new TraceClassVisitor(v, t._1, t._2)

  implicit val fsFoldable: Foldable[({type T[Path] = FileSystem})#T] = new Foldable.FromFoldMap[({type T[Path] = FileSystem})#T] {
    def foldMap[A, B](fa: ({type T[Path] = FileSystem})#T[A])(f: A => B)(implicit F: Monoid[B]) = {
      var res: B = F.zero
      Files.walkFileTree(fa.getPath("/"), new SimpleFileVisitor[Path] {
        override def visitFile(path: Path, attrs: BasicFileAttributes) = {
          res = F.append(res, f(path.asInstanceOf[A]))
          FileVisitResult.CONTINUE
        }
      })
      res
    }
  }

  implicit val FsUnapply: Unapply[Foldable, FileSystem] {
    type M[X] = FileSystem
    type A = Path
  } = new Unapply[Foldable, FileSystem] {
    type M[X] = FileSystem
    type A = Path
    def TC = fsFoldable
    def leibniz = Leibniz.refl
  }

  def toNewFs(path: Path)(fs: FileSystem) =
    fs.getPath("/", path.getFileSystem.getPath("/").relativize(path).toString)

  def fsClassWrite(fs: FileSystem) = {(t: (Option[Array[Byte]], Path)) =>
    val (data, path) = t
    val nPath = toNewFs(path)(fs)
    //println(nPath.toUri)
    Files.createDirectories(nPath.getParent)
    data match {
      case Some(d) =>
        val os = Files.newOutputStream(nPath, SOO.CREATE, SOO.TRUNCATE_EXISTING, SOO.WRITE)
        os.write(d)
        os.close()
      case None => 
        Files.copy(path, nPath, SCO.REPLACE_EXISTING)
    }
  }

  val readClass: Path => ClassReader = { p =>
    new ClassReader(Files.newInputStream(p))
  }

  val nodeClass = (cr: ClassReader) => {
    val node = new ClassNode
    cr.accept(node, 0)
    node
  }

  def writeFixed[P: ClassProvider](tree: SuperTree)(f: ClassVisitor => ClassVisitor) = (p: P) => {
    val cw = new TreeWriter(tree, ClassWriter.COMPUTE_FRAMES)
    implicitly[ClassProvider[P]].point(p)(f(cw))
    cw.toByteArray
  }

  val classFilter = Kleisli[Option, Path, Path](p => if(p.toString.endsWith(".class")) Some(p) else None)

  def transformClasses(inFs: FileSystem, outFs: FileSystem)(tree: SuperTree)(visitor: ClassVisitor => ClassVisitor): Unit = {
    val ls = LensFamily.firstLensFamily[Path, Option[Array[Byte]], Path]
    inFs.toList.fpair map 
      (ls =>= (classFilter map (readClass >>> writeFixed(tree)(visitor))).run) >>> fsClassWrite(outFs)
  }

  val toSuperMaps = (n: ClassNode) => (
    Option(n.superName).map(s => Map(n.name -> Tags.FirstVal(s))).getOrElse(Map.empty),
    Map(n.name -> n.interfaces.asInstanceOf[JList[String]].toSet)
  )

  val toMethodMaps = (n: ClassNode) => {
    val (sMap, iMap) = toSuperMaps(n)
    val mMap = Map(n.name -> (for(m@(_m: MethodNode) <- n.methods) yield MethodAcc(n.name, m.name, m.desc, m.access)).toSet)
    (sMap, iMap, mMap)
  }

  //(Map[String, String @@ Tags.FirstVal], Map[String, Set[String]])
  def foldFiles[F[_]: Foldable, R: Monoid](proc: ClassNode => R)(files: F[Path]): R =
    //(files filter classFilter.isDefinedAt map (readClass >>> nodeClass >>> proc)).fold
    files foldMap classFilter.map(readClass >>> nodeClass >>> proc).run.andThen(_.orZero)

  def genSuperMaps[F[_]: Foldable](files: F[Path]) = foldFiles(toSuperMaps)(files)

  def genMethodMaps[F[_]: Foldable](files: F[Path]) = foldFiles(toMethodMaps)(files)

  def openZip(pathString: String): FileSystem =
    openZip(Paths.get(pathString))

  def openZip(pathString: String, create: Boolean): FileSystem =
    openZip(Paths.get(pathString), create)

  def openZip(path: Path, create: Boolean = false): FileSystem = {
    val uri = URI.create("jar:file:" + path.toUri.getPath)
    FileSystems.newFileSystem(uri, Map("create" -> create.toString))
  }
}


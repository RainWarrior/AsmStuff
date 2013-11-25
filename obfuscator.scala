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

import java.nio.file.{ Files, FileSystem, Path, Paths }
import java.nio.charset.Charset
import java.io.PrintWriter

import scopt.{ Read, OptionDef, OptionParser }

import collection.JavaConversions._

import scalaz._
import Scalaz._

import Util._
import Util.mapMethodMonoid

object TreeObfuscator {

  case class Config(
    fromClasses: Vector[Path] = Vector.empty,
    toClasses: Vector[Path] = Vector.empty,
    forwConverts: Vector[(Path, Path)] = Vector.empty,
    backConverts: Vector[(Path, Path)] = Vector.empty,
    forwSrgs: Vector[Path] = Vector.empty,
    backSrgs: Vector[Path] = Vector.empty,
    saveSrg: Option[Path] = None,
    saveRSrg: Option[Path] = None,
    recheck: Boolean = false
  )

  implicit val pathRead: Read[Path] = Read.reads { Paths.get(_) }

  val parser = new scopt.OptionParser[Config]("tree_obfuscator") {
    def fileOpt(name: String): OptionDef[Path, Config] =
      opt[Path](name) valueName "<jar or dir>"
    def fileSeqOpt(name: String): OptionDef[Path, Config] =
      opt[Path](name) valueName "<jar or dir>" unbounded ()
    def filePairOpt(name: String): OptionDef[(Path, Path), Config] =
      opt[(Path, Path)](name) keyValueName ("<input jar or dir>", "<output jar or dir>")

    head("tree_obfuscator - java obfuscator with method override checking")
    help("help") text "prints this usage text"
    fileSeqOpt("srg") abbr "s" text "add srg file with forward mappings" action { (x, c) =>
      c.copy(forwSrgs = c.forwSrgs :+ x)
    }
    fileSeqOpt("reverse-srg") abbr "rs" text "add srg file with backward mappings" action { (x, c) =>
      c.copy(backSrgs = c.backSrgs :+ x)
    }
    fileSeqOpt("classes-from") abbr "cf" text "add unconverted classes for method lookup" action { (x, c) =>
      c.copy(fromClasses = c.fromClasses :+ x)
    }
    fileSeqOpt("classes-to") abbr "ct" text "add converted classes for method lookup" action { (x, c) =>
      c.copy(toClasses = c.toClasses :+ x)
    }
    filePairOpt("convert") abbr "c" text "convert specified classes using all provided mappings" action { (x, c) =>
      c.copy(forwConverts = c.forwConverts :+ x)
    }
    filePairOpt("reverse-convert") abbr "rc" text "convert specified classes using reverse mappings" action { (x, c) =>
      c.copy(forwConverts = c.forwConverts :+ x)
    }
    fileOpt("save-srg") abbr "ss" text "save resulting srg file" action { (x, c) =>
      c.copy(saveSrg = Some(x))
    }
    fileOpt("save-reverse-srg") abbr "srs" text "save resulting reverse srg file" action { (x, c) =>
      c.copy(saveRSrg = Some(x))
    }
    opt[Unit]("recheck") abbr "r" text "enable additional mapping checks" action { (_, c) =>
      c.copy(recheck = true)
    }
    // TODO load/save tree
  }

  def main(args: Array[String]) {

    parser.parse(args, Config()) map { c =>
      import c._

      if(forwSrgs.length + backSrgs.length == 0) {
        parser.reportError("Need at least one srg file")
        parser.showUsage
        return
      }

      val forwLines = forwSrgs flatMap (p => Files.readAllLines(p, Charset.defaultCharset).to[Vector])
      val backLines = backSrgs flatMap (p => Files.readAllLines(p, Charset.defaultCharset).to[Vector].map(Srg.revLine))

      val forwSrg = forwLines ++ (backLines map Srg.revLine)
      val backSrg = (forwLines map Srg.revLine) ++ backLines

      println("read srg")

      var toClose: List[FileSystem] = Nil

      def open(p: Path): Vector[Path] = {
        val fs = openZip(p)
        toClose = fs :: toClose
        fs.to[Vector]
      }

      val fromTree1 = MapMethodTree(
        genMethodMaps(fromClasses flatMap open)
      )
      val forwMapper1 = MethodPropagatingUnmapper(fromTree1)
      Srg.load(forwSrg, forwMapper1)

      toClose map (_.close())
      toClose = Nil

      println("read classes-from")

      val toTree1 = MapMethodTree(
        genMethodMaps(toClasses flatMap open)
      )
      val backMapper1 = MethodPropagatingUnmapper(toTree1)
      Srg.load(backSrg, backMapper1)

      toClose map (_.close())
      toClose = Nil

      println("read classes-to")

      val fromTree = fromTree1 |+| (toTree1 translate backMapper1)
      val forwMapper = MethodPropagatingUnmapper(fromTree)
      Srg.load(forwMapper1.toSrg, forwMapper)

      println("built forward mapper")

      val toTree = (fromTree1 translate forwMapper1) |+| toTree1
      val backMapper = MethodPropagatingUnmapper(toTree)
      Srg.load(backMapper1.toSrg, backMapper)

      println("built backward mapper")

      if(recheck) {
        val toTree2 = fromTree translate forwMapper
        val fromTree2 = toTree translate backMapper

        if(fromTree.sMap != fromTree2.sMap) {
          println("sMaps1 differ!")
        }
        if(fromTree.iMap != fromTree2.iMap) {
          println("iMaps1 differ!")
        }
        if(fromTree.mMap != fromTree2.mMap) {
          println("mMaps1 differ!")
          //println(fromTree.mMap)
          //println(fromTree2.mMap)
        }

        if(toTree.sMap != toTree2.sMap) {
          println("sMaps2 differ!")
        }
        if(toTree.iMap != toTree2.iMap) {
          println("iMaps2 differ!")
        }
        if(toTree.mMap != toTree2.mMap) {
          println("mMaps2 differ!")
          //println(toTree.mMap)
          //println(toTree2.mMap)
        }

        println("rechecked")

      }

      for(p <- saveSrg) {
        Files.write(p, forwMapper.toSrg, Charset.defaultCharset)
      }

      for(p <- saveRSrg) {
        Files.write(p, backMapper.toSrg, Charset.defaultCharset)
      }

      println("saved srgs")

      for((i, o) <- forwConverts) {
        val iz = openZip(i)
        val oz = openZip(o, true)
        transformClasses(iz, oz)(toTree)(forwMapper)
        iz.close()
        oz.close()
      }

      println("converted forwards")

      for((i, o) <- backConverts) {
        val iz = openZip(i)
        val oz = openZip(o, true)
        transformClasses(iz, oz)(fromTree)(backMapper)
        iz.close()
        oz.close()
      }

      println("converted backwards")
    }

    /*
    //val inZip = openZip("test.jar")
    //val inZip = openZip("1.6.4.jar")
    val inZip = openZip("1.7.2.jar")
    //val outZip = openZip("1.6.4_test.jar", true)

    //val inFiles = inZip.unit[List[Path]]
    val inFiles = inZip.toList

    val mTree = MapMethodTree(genMethodMaps(inFiles))

    //println(provider.mMap)
    //println(provider.fullMMap)

    val unmapper = MethodPropagatingUnmapper(mTree)

    //Srg.fromPath(Paths.get("test.srg"), unmapper)
    //Srg.fromPath(Paths.get("1.6.4_mcp.srg"), unmapper)
    Srg.fromPath(Paths.get("1.7.2_mcpfixed.srg"), unmapper)

    for(f <- Option(new PrintWriter("output.srg"))) { f.write(unmapper.toSrg.mkString("\n")); f.close() }
    //println(unmapper.toSrg.mkString("\n"))
    //transformClasses(inZip, outZip)(provider)(v => v)

    //transformClasses((new ASMifier, new PrintWriter(System.out)))(inZip, outZip)
    */
  }
}


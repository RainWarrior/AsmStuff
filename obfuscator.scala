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

package obfuscator

import java.nio.file.Paths

import asmstuff._
import Util._

import scalaz._
import Scalaz._

object Main {
  def main(args: Array[String]) {

    //val inZip = openZip("test.jar")
    //val inZip = openZip("1.6.4.jar")
    val inZip = openZip("1.7.2.jar")
    //val outZip = openZip("1.6.4_test.jar", true)

    //val inFiles = inZip.unit[List[Path]]
    val inFiles = inZip.toList

    val mTree = new MapMethodTree {
      val (sMap, iMap, mMap) = genMethodMaps(inFiles)
    }

    //println(provider.mMap)
    //println(provider.fullMMap)

    val unmapper = new SimpleUnmapper with MethodPropagatingUnmapper {
      val tree = mTree
    }

    //Srg(Paths.get("test.srg"), unmapper)
    //Srg(Paths.get("1.6.4_mcp.srg"), unmapper)
    Srg.fromPath(Paths.get("1.7.2_mcp.srg"), unmapper)

    //transformClasses(inZip, outZip)(provider)(v => v)

    //transformClasses((new ASMifier, new PrintWriter(System.out)))(inZip, outZip)
  }
}


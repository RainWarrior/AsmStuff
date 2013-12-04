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

package stackmap

import java.nio.file.Path

import scalaz._
import Scalaz._

import asmstuff._
import Util._

object Main {
  def main(args: Array[String]) {

    val inRoot: Path = openZip("1.6.4_mcp.jar").getPath("/")
    val outRoot: Path = openZip("1.6.4_mcp2.jar", true).getPath("/")

    val prov = MapSuperTree(genSuperMaps[WalkUniv](Walk(inRoot)))

    transformClasses(inRoot, outRoot, prov, c => c)(v => v)

    //transformClasses((new ASMifier, new PrintWriter(System.out)))(inZip, outZip)
  }
}


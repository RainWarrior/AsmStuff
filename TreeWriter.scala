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

import org.objectweb.asm.ClassWriter

import annotation.tailrec

import Types._

trait SuperTreeWriter extends ClassWriter {
  val tree: SuperTree

  override def getCommonSuperClass(t1: ClassT, t2: ClassT): ClassT = {
    @tailrec def rec(t1: ClassT, t2: ClassT): ClassT = {
      if(isAssignableFrom(t1, t2)) t1
      else if(isAssignableFrom(t2, t1)) t2
      else tree.getSuper(t1) match {
        case Some(t3) => rec(t3, t2)
        case _ => "java/lang/Object"
      }
    }
    rec(t1, t2)
  }

  def isAssignableFrom(t1: ClassT, t2: ClassT): Boolean = {
    if(t1 == t2) true
    else if(t2 == "java/lang/Object") false
    else tree.getParents(t2).exists(t3 => isAssignableFrom(t1, t3))
  }
}

class TreeWriter(val tree: SuperTree, flags: Int) extends ClassWriter(flags) with SuperTreeWriter


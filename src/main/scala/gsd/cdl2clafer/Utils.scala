/*
 * Copyright (c) 2010 Marko Novakovic <mnovakov@gsd.uwaterloo.ca>
 *
 * This file is part of cdl2clafer.
 *
 * cdl2clafer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * cdl2clafer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with cdl2clafer.  If not, see <http://www.gnu.org/licenses/>.
 */
 
package gsd.cdl2clafer

import gsd.cdl.model._
import gsd.cdl2clafer.model._

object Utils {

  /**
    * Guards identifier name, because Clafer parser cannot handle 
    * certain characters, like '_'
    */  
  def guardIdentifier(id:String):String = {
    id.replaceAll("_", "_")
  }
  
/* 
  **
   * nodes are first-level nodes that have hierarchy included
   *
  def getMandatorityMap(nodes:List[Node]):scala.collection.immutable.Map[String, Boolean] = {
   var map = scala.collection.mutable.Map[String, Boolean]()
   nodes.foreach(node => {
    setMandatority(node, map)
   })
   
   scala.collection.immutable.Map[String, Boolean]() ++ map
  }
  
  def setMandatority(node:Node, map:scala.collection.mutable.Map[String, Boolean]):scala.collection.mutable.Map[String, Boolean] = {
   if (isMandatoryLocally(node)) {
    map += (node.id -> true)
   } else {
    map += (node.id -> false)
   }
   
   node.children.foreach(c => setMandatority(c, map))
   map
  }
 */
}

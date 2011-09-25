/*
 * Copyright (c) 2011 Marko Novakovic <mnovakov@gsd.uwaterloo.ca>
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
import gsd.cdl.formula._
import gsd.cdl2clafer.model._
import gsd.cdl2clafer.utils._
import org.kiama.rewriting.Rewriter._
import gsd.cdl.formula.types.checker._
import java.io.File
import java.io.FileFilter

object QueryAllFeaturesInDir {

  def apply() = {

     val fs = new File("input/iml/").listFiles(new FileFilter() {
        def accept(pathname:File) = pathname.getName.endsWith(".iml")
     })

    var ids = scala.collection.mutable.Set[String]()

     for(f <- fs) {
	    val nodes = gsd.cdl2clafer.parser.EcosIML.parseFile(f.getAbsolutePath())
	 
	    val allNodes:List[Node] = collectl { case n:Node => n }(nodes).map(_ match {
	        case 	Node(id, PackageType, display, description, flavor,     defaultValues, calculated, legalValues, reqs, activeIfs, implements, children) => 
	        		Node(id, PackageType, display, description, NoneFlavor, defaultValues, calculated, legalValues, reqs, activeIfs, implements, children)
	        case x@_ => x
         })
         val allNodesMap:scala.collection.immutable.Map[String, Node] = allNodes.map(n => (n.id, n)).toMap[String, Node]         
         
        allNodes.foreach(node => {
          node.activeIfs.foreach(c => {
            if (countInterfaceOccurences(c, allNodesMap) != 0) {
              if (countCDLExpressions(c) > 3) {
                println("--------------------------------")
            	println(c)
              }
            }
          })
          node.reqs.foreach(c => {
            if (countInterfaceOccurences(c, allNodesMap) != 0) {
              if (countCDLExpressions(c) > 3) {
                println("--------------------------------")
            	println(c)
              }
            }
          })
          
          if (node.calculated != None) {
            if (countInterfaceOccurences(node.calculated.getOrElse(null), allNodesMap) != 0) {
              if (countCDLExpressions(node.calculated.getOrElse(null)) > 3) {
                  println("--------------------------------")
            	  println(node.calculated.getOrElse(null))
              }
            }
          }
        })
     }
    
    ids.foreach(println)
  }
  
	def countInterfaceOccurences(constraint:CDLExpression, 
	   allNodesMap:scala.collection.immutable.Map[String, Node]):Int = {
		count { case gVar:Identifier => {
		  if (allNodesMap.contains(truncateVarNames(gVar.id))) {
		    if (allNodesMap.apply(truncateVarNames(gVar.id)).cdlType == InterfaceType) {
		      1
		    } else {
		      0
		    }
		  } else {
		    0
		  }
		 }}(constraint)
	}
	
  	def countCDLExpressions(constraint:CDLExpression):Int = {
		count { 
		  case x@_ => (if (x.isInstanceOf[CDLExpression]) 1 else 0)
		}(constraint)
	}


 def truncateVarNames(variable:String):String = {
   variable.replaceAll("_bool_var", "").
   replaceAll("_data_var", "")
 } 
}
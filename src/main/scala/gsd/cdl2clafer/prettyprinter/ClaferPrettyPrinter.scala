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

/**
 * Prints Clafer Nodes 
 ***/
object ClaferPrettyPrinter extends org.kiama.util.PrettyPrinter {

//    import AST._
    
    def pretty(node: ClaferNode) : String = pretty(node, 0)

    
    def pretty(node: ClaferNode, depth:Int) : String =
        super.pretty (showClaferNode (node, depth))

    /**
     * Printing with children included (if any)
     **/
    def showClaferNode(node:ClaferNode, depth: Int): Doc = {
      if (node.children.size == 0) {
        nest(
        		showClaferNodeWithoutChildren(node, depth),
        		depth + 1
        )
      } else {
    	nest(
    	    showClaferNodeWithoutChildren(node, depth) 
    	    <@>
    	    vsep(node.children.toSeq.map(a => {showClaferNode(a, depth)})), 
    	    depth + 1
    	)
      }
    }
    
    /**
     * We separate printing of children from printing of a node itself
     * 
     *  This represents only node -- without it's children
     **/
    def showClaferNodeWithoutChildren(node:ClaferNode, depth:Int):Doc = {
      
      if (node.constraints.size > 0) {
//	      nest(
	          firstLineOfClaferAndImplements(node) <@>
		      vsep(node.constraints.map(c => { 
		        brackets(
		    		  text (GExpressionPrettyPrinter.pretty(c))
		        )
		      }))
//		      depth + 1
//	      )
      } else {
        firstLineOfClaferAndImplements(node)
      }
    }
    
    private def implements(node:ClaferNode):Doc = {
      if (node.implements.size > 0) {
    	  	  empty <@>
		      vsep(node.implements.map(c => {
		    	text("`") <> text (GExpressionPrettyPrinter.pretty(c))
		      })) 
	  } else 
        empty
    } 
    
    /**
     * Represents the first line of a Clafer node 
     **/
    private def firstLineOfClaferAndImplements(node:ClaferNode):Doc =  {
      var doc = empty
      if (node.isAbstract) {
        doc = doc <> text("abstract") <+> empty 
      }
      doc = doc <> text(node.name)
      if (node.claferType != types.BoolType && !node.isAbstract) {
        node.claferType match {
          case DisjunctiveType => doc = doc <+> text("-> ambigious")
          case types.NumberType => doc = doc <+> text("-> int")
          case types.StringType => doc = doc <+> text("-> string")
          case UndefinedType => doc = doc <+> text("-> undefined")
        }
      }
      if (!node.isMandatory) {
    	  doc = doc <+> text("?") 
      }
      
      // TODO: HOT TO DEAL WITH IMPLEMENTATIONS?

      doc <> implements(node)
    }
}

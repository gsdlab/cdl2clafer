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

package gsd.cdl2clafer.utils

import gsd.cdl.model._
import gsd.cdl.formula._

import gsd.cdl2clafer._
import gsd.cdl2clafer.model._

import gsd.cdl.formula.types._

import org.kiama.rewriting._
import org.kiama.rewriting.Rewriter._

import scala.collection._

object Rewriters {
  
   private def truncateVarNames(variable:String):String = {
       val withoutDataPart = DataVariableName.unapply(variable).getOrElse(variable)
       BoolVariableName.unapply(withoutDataPart).getOrElse(withoutDataPart)
   }
   
   private def replaceGCastsInGVariable(exp:GVariable, castExpression:GExpression):GExpression = {
	    if (exp.getType().head == BoolType && 
	             exp.getRequiredType().getOrElse(null) == NumberType) {
	           // bool to number conversion, for example A + 1, where A is BoolFlavor
	           GConditional(GVariable(exp.asInstanceOf[GVariable].id), GLongIntLiteral(1), GLongIntLiteral(0))
	    } else if (exp.getType().head == BoolType && 
	             exp.getRequiredType().getOrElse(null) == StringType) {
	         	// string to number conversion, A == "1", where A is String
	         	// this is converted to A ? "1" : "0"
	    	   GConditional(GVariable(exp.asInstanceOf[GVariable].id), GStringLiteral("1"), GStringLiteral("0"))
	    } else if (exp.getType().head == NumberType && 
	             exp.getRequiredType().getOrElse(null) == BoolType) {
	         	// number to boolean conversion, A ? 1: 2, where A is Number
	         	// this is converted to A <> 0
	    	   GNot(GEq(GVariable(exp.asInstanceOf[GVariable].id), GLongIntLiteral(0)))
	    } else if (exp.getType().head == StringType && 
	             exp.getRequiredType().getOrElse(null) == BoolType) {
	         	// string to boolean conversion, A ? 1: 2, where A is String
	         	// this is converted to A <> "0"
	    	   GNot(GEq(GVariable(exp.asInstanceOf[GVariable].id), GStringLiteral("0")))
	    } else {
	      castExpression
	    }
   }
   
   private def replaceGCastsInLongIntLiterals(exp:GLongIntLiteral, castExpression:GExpression):GExpression = {
       if (exp.getType().head == NumberType && 
	             exp.getRequiredType().getOrElse(null) == BoolType) {
         // Literal to boolean conversion
         if (exp.asInstanceOf[GLongIntLiteral].value != 0) {
           GTrue()
         } else {
           GFalse()
         }
       } else if (exp.getType().head == NumberType && 
	     exp.getRequiredType().getOrElse(null) == StringType) {
         // conversion of int literal to string
         // 1200 converted to "1200"
         // 0 to "0"
           GStringLiteral("" + exp.value)
       } else {
         castExpression
       }
   }
   
   def replaceGCasts(exp:GExpression):GExpression = {
	   val rule = rulef {
		   case variable@GCast(exp) => {
			   if ((exp.getType().size == 1) && exp.getRequiredType() != None) {
			     if (exp.isInstanceOf[GVariable]) {
			       replaceGCastsInGVariable(exp.asInstanceOf[GVariable], variable)
			     } else 
			       if (exp.isInstanceOf[GLongIntLiteral]) {
			       replaceGCastsInLongIntLiterals(exp.asInstanceOf[GLongIntLiteral], variable)
			     } else
			       variable
			    } else {
			      variable
			   }
		   }
		   case x@_ => x
	   }
	   
	   rewriteS(exp, rule)
   }
   
   def applyGuardings(parentId:String,
		   exp:GExpression,
    	    allNodesMap:scala.collection.immutable.Map[String, Node],
    	    symbolTable:mutable.Map[String, mutable.Set[Type]]
   ):GExpression = {
	   val rule = rulef {
		   case variable@GVariable(id) => {
		     if (parentId != id) {
			   if (allNodesMap.apply(id).cdlType != InterfaceType) {
			     if (allNodesMap.apply(id).flavor == DataFlavor ||
			         allNodesMap.apply(id).flavor == BoolDataFlavor) {
			       if (!symbolTable.contains(id) || symbolTable.apply(id).size != 1 ||
			           (symbolTable.apply(id).head != NumberType &&
			           symbolTable.apply(id).head != StringType)) {
			         throw new Exception("There is more than one type or the type is wrong: " + variable)
			       } else {
			         if (symbolTable.apply(id).head == NumberType) {
			        	 GConditional(variable, variable, GLongIntLiteral(0))
			         } else {
			        	 GConditional(variable, variable, GStringLiteral("0"))
			         }
			       }
			     } else variable
			       
			   } else {
			    if (allNodesMap.apply(id).flavor != BoolFlavor) {
			      // variables that represent interfaces
			      GConditional(variable, variable, GLongIntLiteral(0))
			    } else variable
			   }
		     } else {
		       GVariable("this")   
		     }
		   }
		   case x@_ => x
	   }
	   
	   rewriteS(exp, rule)
   }   

 	def rewriteGVariable(exp:GExpression):GExpression = {
	   val rewriteGVariableNameRule = rulef {
	   case variable@GVariable (v) => {
		   getGVariableWithName(variable, truncateVarNames(v))
	   }
	   case x@_ => x
	   }

	   rewriteS(exp, rewriteGVariableNameRule)
 	} 
 	
 	def rewriteS(exp:GExpression, s:Strategy):GExpression = {
 	  rewrite(GRewriter.geverywherebu(s))(exp)
 	}
 	
 	def addInterfaceSuffix(exp:GExpression,
 	    allNodesMap:scala.collection.immutable.Map[String, Node]
 	):GExpression = {
	 	val rule = rulef {
	    	case variable@GVariable (v) => {
	    	  if (allNodesMap.contains(truncateVarNames(v)) 
	    	      && allNodesMap.apply(truncateVarNames(v)).cdlType == InterfaceType
	    	      && allNodesMap.apply(truncateVarNames(v)).flavor != BoolFlavor
	    	      ) {
			    getGVariableWithName(variable, DataVariableName(DataVariableName.unapply(v).getOrElse(v) + "_INTERFACE_ARTIFICIALLY_ADDED"))
	    	  } else {
	    	    variable
	    	  }
	    	}
	    	case x@_ => x
	    }
	 	
	 	rewriteS(exp, rule)
 	}
 	
 	def removeInterfaceSuffix(exp:GExpression):GExpression = {
	 	val rule = rulef {
	    	case variable@GVariable (v) => {
	    	  ArtificialInterfaceVariableName.unapply(v) match {
	    	    case None => variable
	    	    case Some(string) => getGVariableWithName(variable, string) 
	    	  }
	    	}
	    	case x@_ => x
	    }
	 	
	 	rewriteS(exp, rule)
 	} 
 	
 	private def getGVariableWithName(old:GVariable, name:String):GVariable = {
		var newGVar:GVariable = GVariable(name)
		newGVar.setRequiredType(old.getRequiredType())
		newGVar.setType(old.getType())
	    val vv = newGVar
	    vv
 	} 	
}

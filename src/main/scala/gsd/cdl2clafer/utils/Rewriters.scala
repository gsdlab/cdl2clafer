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
//	           println("EXAMPLE OF BOOL TO NUMBER TRANSLATION: " + exp)
	           GConditional(GVariable(exp.asInstanceOf[GVariable].id), GLongIntLiteral(1), GLongIntLiteral(0))
	    } else if (exp.getType().head == BoolType && 
	             exp.getRequiredType().getOrElse(null) == StringType) {
	         	// string to number conversion, A == "1", where A is String
	         	// this is converted to A ? "1" : "0"
//	           println("EXAMPLE OF BOOL TO STRING TRANSLATION: " + exp)
	    	   GConditional(GVariable(exp.asInstanceOf[GVariable].id), GStringLiteral("1"), GStringLiteral("0"))
	    } else if (exp.getType().head == NumberType && 
	             exp.getRequiredType().getOrElse(null) == BoolType) {
	         	// number to boolean conversion, A ? 1: 2, where A is Number
	         	// this is converted to A <> 0
//	           println("EXAMPLE OF NUMBER TO BOOL TRANSLATION: " + exp)
	    	   GNot(GEq(GVariable(exp.asInstanceOf[GVariable].id), GLongIntLiteral(0)))
	    } else if (exp.getType().head == StringType && 
	             exp.getRequiredType().getOrElse(null) == BoolType) {
	         	// string to boolean conversion, A ? 1: 2, where A is String
	         	// this is converted to A <> "0"
//	           println("EXAMPLE OF STRING TO BOOL TRANSLATION: " + exp)
	    	   GNot(GEq(GVariable(exp.asInstanceOf[GVariable].id), GStringLiteral("0")))
	    } else if (exp.getType().head == StringType && 
	             exp.getRequiredType().getOrElse(null) == NumberType) {
	    	throw new Exception("String to Number VARIABLE Conversion!")
	    } else if (exp.getType().head == NumberType && 
	             exp.getRequiredType().getOrElse(null) == StringType) {
	    	throw new Exception("Number to String VARIABLE Conversion!")
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
   
   private def replaceGCastsInStringLiterals(exp:GStringLiteral, castExpression:GExpression):GExpression = {
       if (exp.getType().head == StringType && 
	             exp.getRequiredType().getOrElse(null) == BoolType) {
         // Literal to boolean conversion
         if (exp.asInstanceOf[GStringLiteral].value != "0" || exp.asInstanceOf[GStringLiteral].value != "") {
           GTrue()
         } else {
           GFalse()
         }
       } else if (exp.getType().head == StringType && 
	     exp.getRequiredType().getOrElse(null) == types.NumberType) {
         // conversion of string literal to int
         throw new Exception("String to Number Conversion")
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
			     } else if (exp.isInstanceOf[GLongIntLiteral]) {
			       replaceGCastsInLongIntLiterals(exp.asInstanceOf[GLongIntLiteral], variable)
			     } else if (exp.isInstanceOf[GStringLiteral]) {
			       replaceGCastsInStringLiterals(exp.asInstanceOf[GStringLiteral], variable)
			     } else
			       exp match {
			        // TODO: Make This Work
//			       	case GBtOr(left, right)  => GConditional(GEq(GBtOr(left, right), GLongIntLiteral(0)), GFalse(), GTrue())
//			       	case GBtXor(left, right) => GConditional(GEq(exp, GLongIntLiteral(0)), GFalse(), GTrue())
			       	case _ => variable
			       } 
			    } else {
			      if (exp.getType().size != 1) {
			        throw new Exception("More than one expected type!")
			      }
			      if (exp.getRequiredType() == None) {
			        throw new Exception("Unknown required type")
			      }
			      variable
			   }
		   }
		   case x@_ => x
	   }
	   
	   rewritebu(exp, rule)
   }
   
   def convertConditionals(exp:GExpression):GExpression = {
	   val rule = rulef {
	   case variable@GEq(left, GConditional(cond, pass, fail)) => {
		   GConditional(cond, GEq(left, pass), GEq(left, fail))
	   }
	   case variable@GEq(GConditional(cond, pass, fail), right) => {
		   GConditional(cond, GEq(pass, right), GEq(fail, right))
	   }
	   // >=
	   case variable@GGreaterEqThan(left, GConditional(cond, pass, fail)) => {
		   GConditional(cond, GGreaterEqThan(left, pass), GGreaterEqThan(left, fail))
	   }
	   case variable@GGreaterEqThan(GConditional(cond, pass, fail), right) => {
		   GConditional(cond, GGreaterEqThan(pass, right), GGreaterEqThan(fail, right))
	   }
	   // <=
	   case variable@GLessEqThan(left, GConditional(cond, pass, fail)) => {
		   GConditional(cond, GLessEqThan(left, pass), GLessEqThan(left, fail))
	   }
	   case variable@GLessEqThan(GConditional(cond, pass, fail), right) => {
		   GConditional(cond, GLessEqThan(pass, right), GLessEqThan(fail, right))
	   }
	   // >
	   case variable@GGreaterThan(left, GConditional(cond, pass, fail)) => {
		   GConditional(cond, GGreaterThan(left, pass), GGreaterEqThan(left, fail))
	   }
	   case variable@GGreaterThan(GConditional(cond, pass, fail), right) => {
		   GConditional(cond, GGreaterThan(pass, right), GGreaterThan(fail, right))
	   }
	   // <
	   case variable@GLessThan(left, GConditional(cond, pass, fail)) => {
		   GConditional(cond, GLessThan(left, pass), GLessThan(left, fail))
	   }
	   case variable@GLessThan(GConditional(cond, pass, fail), right) => {
		   GConditional(cond, GLessThan(pass, right), GLessThan(fail, right))
	   }
	   // - 
	   case variable@GMinus(left, GConditional(cond, pass, fail)) => {
		   GConditional(cond, GMinus(left, pass), GMinus(left, fail))
	   }
	   case variable@GMinus(GConditional(cond, pass, fail), right) => {
		   GConditional(cond, GMinus(pass, right), GMinus(fail, right))
	   }
	   // +
	   case variable@GPlus(left, GConditional(cond, pass, fail)) => {
		   GConditional(cond, GPlus(left, pass), GPlus(left, fail))
	   }
	   case variable@GPlus(GConditional(cond, pass, fail), right) => {
		   GConditional(cond, GPlus(pass, right), GPlus(fail, right))
	   }
	   // Contatenation
	   case variable@GDot(left, GConditional(cond, pass, fail)) => {
		   GConditional(cond, GDot(left, pass), GDot(left, fail))
	   }
	   case variable@GDot(GConditional(cond, pass, fail), right) => {
		   GConditional(cond, GDot(pass, right), GDot(fail, right))
	   }
	   case x@_ => x
	   }
	   
	   rewritebu(exp, rule)
   }
   
   //CYGSEM_KERNEL_SYNCH_MUTEX_PRIORITY_INVERSION_PROTOCOL_DEFAULT_PRIORITY
   
   def applyGuardings(parentId:String,
		   exp:GExpression,
    	    allNodesMap:scala.collection.immutable.Map[String, Node],
    	    symbolTable:mutable.Map[String, mutable.Set[Type]]
   ):GExpression = {
	   val rule = rulef {
		   case variable@GVariable(id) => {
		     
		     
		     if (parentId != id) { // does this var. represents it's parent node
		       // non interfaces
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
			    //Variables representing Interfaces
			    if (allNodesMap.apply(id).flavor != BoolFlavor) {
			      // variables that represent interfaces
			      GConditional(variable, variable, GLongIntLiteral(0))
			    } else {
			      variable
			      }
			   }
		     } else {
		       GVariable("this")   
		     }
		   }
		   case x@_ => x
	   }
	   
	   rewritebu(exp, rule)
   }   

 	def rewriteGVariable(exp:GExpression):GExpression = {
	   val rewriteGVariableNameRule = rulef {
	   case variable@GVariable (v) => {
		   getGVariableWithName(variable, truncateVarNames(v))
	   }
	   case x@_ => x
	   }

	   rewritebu(exp, rewriteGVariableNameRule)
 	} 
 	
 	private def rewritebu(exp:GExpression, s:Strategy):GExpression = {
 	  rewrite(GRewriter.geverywherebu(s))(exp)
 	}

 	private def rewritetd(exp:GExpression, s:Strategy):GExpression = {
 	  rewrite(GRewriter.geverywheretd(s))(exp)
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
	 	
	 	rewritebu(exp, rule)
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
	 	
	 	rewritebu(exp, rule)
 	} 
 	
 	private def getGVariableWithName(old:GVariable, name:String):GVariable = {
		var newGVar:GVariable = GVariable(name)
		newGVar.setRequiredType(old.getRequiredType())
		newGVar.setType(old.getType())
	    val vv = newGVar
	    vv
 	} 	
}

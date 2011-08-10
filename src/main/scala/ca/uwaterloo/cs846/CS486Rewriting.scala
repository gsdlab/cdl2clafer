package ca.uwaterloo.cs846

import gsd.cdl.model._
import gsd.cdl.formula._
import gsd.cdl2clafer.model._
import gsd.cdl2clafer._
import org.kiama._
import org.kiama.rewriting.Rewriter._

/**
 * A sample rewriting rule for CS846 class demo
 **/
object CS486Rewriting {
  def apply(exp:GExpression):GExpression = {
	 	val rule = rulef {
	 	    case GEq(left, GConditional(c, p, f)) => {
	 	        GConditional(c, GEq(left, p), GEq(left, f))
	 	    }
	    	// don't do any rewritings
	    	case x@_ => x
	    }
	    
//	    throw
	 	
	 	rewrite(rule, exp)
  }
  
  
  
  
  private def rewrite(rule:Strategy, exp:GExpression):GExpression = {
	   topdown(rule)(topdown(rule)(exp).getOrElse(null).asInstanceOf[GExpression]).
	   getOrElse(null).asInstanceOf[GExpression]
  }

}


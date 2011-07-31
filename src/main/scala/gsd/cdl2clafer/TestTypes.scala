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
import gsd.cdl.formula.types.Type

object TestTypes {
  
  
  def all() {
         val fs = new java.io.File("input/iml/").listFiles(new java.io.FileFilter() {
        def accept(pathname:java.io.File) = pathname.getName.endsWith(".iml")
     })

    var ids = scala.collection.mutable.Set[String]()
    
    if (fs.size == 0) {
      throw new Exception("No files")
    }
    	
     for(f <- fs) {
    	 println("Processing: *" + f.getName() + "*")
    	 try {
//       Converter.convert(f.getAbsolutePath())
    		 Converter.printIMLAsClafer(f.getAbsolutePath())
    	 } catch {
    	   case e: Exception => println("error")
    	 }
     }
  }

   
  def writeAll() {
     val fs = new java.io.File("input/iml/").listFiles(new java.io.FileFilter() {
        def accept(pathname:java.io.File) = pathname.getName.endsWith(".iml")
     })

    if (fs.size == 0) {
      throw new Exception("No files")
    }

     var ids = scala.collection.mutable.Set[String]()
    	
     for(f <- fs) {
       if (!f.getAbsolutePath().endsWith(".iml"))
         throw new Exception("Deals with .iml files only")

       val out = new java.io.FileWriter("output/clafer/" + f.getName.substring(0, f.getName.length() - 3) + "cfr")
       println("Processing: *" + f.getAbsolutePath() + "*")
		try {
			Converter.convert(f.getAbsolutePath()).foreach(n => {
				out.write(ClaferPrettyPrinter.pretty(n))
				out.write("\n")
				out.flush
			})
		} catch {
		   case e: Exception => e.printStackTrace()
		}
       
       out.close
     }
  }
  
 def countInterfaceOccurences(constraint:GExpression, 
     allNodesMap:scala.collection.immutable.Map[String, Node]):Int = {
	count { case gVar:GVariable => {
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
 
 def truncateVarNames(variable:String):String = {
   variable.replaceAll("_bool_var", "").
   replaceAll("_data_var", "")
 }
 
 def testCollectl() = {
   val exp1 = GEq(GVariable("123_bool_var"), GLongIntLiteral(1))
   val exp2 = GEq(GConditional(
		   		GGreaterThan(GLongIntLiteral(3), GLongIntLiteral(2)),
		   		GLongIntLiteral(3),
		   		GLongIntLiteral(2)
		   ), 
		   GLongIntLiteral(1)
   )

		   
   	val file = "/home/marko/workspaces/gsdlab/gdansk/xcdl-analysis-dev_1.2/input/extracted_representative_model-cleaned.iml"
    val nodes = gsd.cdl.parser.EcosIML.parseFile(file)
    
    val allNodes:List[CDLExpression] = collectl { case e:Eq => e }(nodes)
    println(allNodes.size)
      
	val counter = count { case exp:GExpression => 1 }
	  
	println(counter(exp1))
	println(counter(exp2))
	
	val r1 = rulef {
    	case GVariable (v) => GVariable(truncateVarNames(v))
    	case GGreaterThan(GLongIntLiteral(left), GLongIntLiteral(right)) => {
    	  if (left > right)
    	    GLongIntLiteral(1)
    	  else 
    	    GLongIntLiteral(0)
    	}
    	case GConditional(GLongIntLiteral(cond), pass, fail) => {
    	  if (cond > 0) {
    	    pass
    	  } else {
    	    fail
    	  }
    	}
    	case GEq(GLongIntLiteral(left), GLongIntLiteral(right)) => {
    	  if (left == right)
    	    GLongIntLiteral(1)
    	  else
    	    GLongIntLiteral(0)
    	} 
    	case x@_ => x
    }
	
	val normal = everywherebu (r1)
	
//	rewrite(geverywherebu(rulef
	
	println(normal(exp1))
	println(normal(exp2))
	
	
 }
 
 
 def testCounting() = {
  	def countCDLExpressions(constraint:CDLExpression):Int = {
		count { 
		  case x@_ => (if (x.isInstanceOf[CDLExpression]) 1 else 0)
		}(constraint)
	}
  	
  	println(countCDLExpressions(Eq(Identifier("A"), LongIntLiteral(1))))
   
 }
}

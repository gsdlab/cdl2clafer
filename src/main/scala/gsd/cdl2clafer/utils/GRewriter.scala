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


/**
 * Copied from Yingfei's GRewriter 
 * 
 * This is nothing but org.kiama.rewriting.Rewriter, with the 
 * exception that we needed to add few lines of code that will
 * copy GExpression's inner fields when creating duplicates of objects.
 * 
 * By default, Kiama only executes default constructor
 * when creating copies of files, with the effect that inner fields
 * will be lost in rewriting 
 * 
 **/

object GRewriter {
  
    def geverywheretd (s : => Strategy) : Strategy =
		gtopdown (attempt (s)) 
		
    def geverywherebu (s : => Strategy) : Strategy =
        gbottomup (attempt (s)) 		

    def gbottomup (s : => Strategy) : Strategy =
        gall (gbottomup (s)) <* s 		
		
    def gtopdown (s : => Strategy) : Strategy =
            s <* gall (gtopdown (s))

    def gall (s : => Strategy) : Strategy =
            new Strategy {
                def apply (t : Term) : Option[Term] =
                        t match {
                                case p : GExpression => gallProduct (p, s)
                                case a           => Some (a)
                        }
            }

	private def gallProduct (p : GExpression, s : => Strategy) : Option[Term] = {
		val numchildren = p.asInstanceOf[Product].productArity
		if (numchildren == 0) {
			Some (p)
		} else {
			val children = new Array[AnyRef](numchildren)
			var hasGExpr = false
			for (i <- 0 until numchildren) {
				val ct = p.asInstanceOf[Product].productElement (i)
				if (ct.isInstanceOf[GExpression]) hasGExpr = true
				s (ct) match {
					case Some (ti) =>
						children (i) = makechild (ti)
					case None      =>
						return None
				}
			}
			if (hasGExpr) {
                            val ret = gdup (p, children)
                            Some (ret)
			}
			else Some (p)
		}
	} 	
	
	private def gdup (t : GExpression, children : Array[AnyRef]) : GExpression = {
        val ctor = (t.getClass.getConstructors())(0)
        try {
            // we need to manually copy inner fields of GExpression
        	// because otherwise only default constructor will be 
        	// executed
            var new1 = ctor.newInstance (children : _*).asInstanceOf[GExpression]
            new1.asInstanceOf[GExpression].setRequiredType(t.getRequiredType())
            new1.asInstanceOf[GExpression].setType(t.getType())
            new1
        } catch {
            case e : java.lang.ClassCastException =>
                error ("dup cast failed: " + t)
            case e : IllegalArgumentException =>
                error ("dup illegal arguments: " + ctor.toString + " (" +
                       children.deep.mkString (",") + "), expects " +
                       ctor.getParameterTypes.length)
        }
    } 
	
	private def makechild (child : Any) : AnyRef = {
        try {
            return child.asInstanceOf[AnyRef]
        } catch {
            case e : ClassCastException =>
                error ("makechild: can't cast child: " + child + " " + e)
        }
    } 
}

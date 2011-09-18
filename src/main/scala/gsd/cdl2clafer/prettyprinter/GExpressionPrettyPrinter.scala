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
 * Prints GExpression Nodes 
 ***/
object GExpressionPrettyPrinter extends org.kiama.util.PrettyPrinter {

    /**
     * GExpression as String  
     **/
    def pretty(t : GExpression) : String = super.pretty(show(t, 0))
    
    /**
     * Represents GExpression 
     **/
    def show (exp : GExpression, depth:Int) : Doc =
        exp match {
            case GLongIntLiteral (d)      => value (d)
            case GStringLiteral (d)      => text ("\"" + d + "\"")
            case GVariable (s)      => text (s)
            case GClaferNoInstances (e)   => text("no") <+> show(e, depth)
            case GNot (e)      => parens (text ("~") <> show (e, depth + 1))
            case GPlus (l, r)   => showbin (l, "+", r, depth + 1)
            case GMinus (l, r)   => showbin (l, "-", r, depth + 1)
            case GTimes (l, r)   => showbin (l, "*", r, depth + 1)
            case GDivide (l, r)   => showbin (l, "/", r, depth + 1)
            case GEq (l, r)   => showbin (l, "==", r, depth + 1)
            case GAnd (l, r)   => showbin (l, "&&", r, depth + 1)
            case GOr (l, r)   => showbin (l, "||", r, depth + 1)
            case GGreaterThan (l, r)   => showbin (l, ">", r, depth + 1)
            case GGreaterEqThan (l, r)   => showbin (l, ">=", r, depth + 1)
            case GLessThan (l, r)   => showbin (l, "<", r, depth + 1)
            case GLessEqThan (l, r)   => showbin (l, "<=", r, depth + 1)
            case GImplies (l, r)   => showbin (l, "=>", r, depth + 1)
            case GEquivalent (l, r)   => showbin (l, "<=>", r, depth + 1)
            case GNumImplementations (v)   => text("#") <> show(v, depth)
            case GDot (l, r)   => showbin (l, "++", r, depth + 1)
            case GConditional (c, p, f)   => parens(show (c, depth + 1) <+> text("=>") <+> show(p, depth + 1) <+> text("else") <+> show(f, depth + 1))
            case GSubString(a, b) => text("isSubstring") <> parens(show(a, depth + 1) <+> text(",") <+> show(b, depth + 1))
            case cast@gsd.cdl.formula.types.GCast(exp1) => text("(") <> text(GExpressionToString(cast)) <> text(")") <> show(exp1, depth + 1)
            case GTrue() => text("true")
            case GFalse() => text("false")
            case GBtAnd(l, r) => text("&")
            case GBtOr(l, r) => showbin (l, "|", r, depth)
            case GBtLeft(l, r) => text("<<")
            case GBtRight(l, r) => text(">>")
            case GBtXor(l, r) => text("XOR")
            case GMod(l, r) => text("%")
            case x@_ => throw new Exception ("Not supported: " + x.getClass.toString)//text(x.getClass().toString())
        }
//    
    def showbin (l : GExpression, op : String, r : GExpression, depth:Int) : Doc = {
      if (depth > 1) {
        parens (show (l, depth + 1) <+> text (op) <+> show (r, depth + 1))
      } else {
        show (l, depth + 1) <+> text (op) <+> show (r, depth + 1)
      }
    }
}

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
import gsd.cdl.formula.Main._
import gsd.cdl2clafer.model._

/**
 * Simple, one-to-one mapping from CDLExpression to GExpression
 * Other mapping and transformations use
 */
object CDLToGExpression {

 def apply(expr:CDLExpression, allNodesMap:scala.collection.immutable.Map[String, Node]):GExpression = {
      expr match {
        case StringLiteral(v) =>
            val numberPattern = """0[xX]([0123456789abcdefABCDEF]+)""".r
            numberPattern.unapplySeq(v) match {
              case Some(List(numberPart)) => GLongIntLiteral(java.lang.Long.parseLong(numberPart, 16).toLong)
              case None => {
                  new GStringLiteral(v)
                }
            }
        case LongIntLiteral(v) => GLongIntLiteral(v)
        case Identifier(v) => { 
          allNodesMap.get(v) match {
            case Some(node) => 
              node.flavor match {
                case DataFlavor     => GVariable(DataVariableName(node.id))
                case BoolDataFlavor => GVariable(DataVariableName(node.id))
                case BoolFlavor     => GVariable(BoolVariableName(node.id))
                case NoneFlavor     => GVariable(BoolVariableName(node.id))
              }
            case None          => GLongIntLiteral(0) //mnovakovic: check this!
          }        
        }
        case Conditional(cond, pass, fail) => GConditional(CDLToGExpression(cond, allNodesMap), CDLToGExpression(pass, allNodesMap), CDLToGExpression(fail, allNodesMap))
        case Or(left, right) => GOr(CDLToGExpression(left, allNodesMap), CDLToGExpression(right, allNodesMap))
        case And(left, right) => GAnd(CDLToGExpression(left, allNodesMap), CDLToGExpression(right, allNodesMap))
        case Eq(left, right) => GEq(CDLToGExpression(left, allNodesMap), CDLToGExpression(right, allNodesMap))
        case NEq(left, right) => GNot(GEq(CDLToGExpression(left, allNodesMap), CDLToGExpression(right, allNodesMap)))
        case LessThan(left, right) => GLessThan(CDLToGExpression(left, allNodesMap), CDLToGExpression(right, allNodesMap))
        case LessThanOrEq(left, right) => GLessEqThan(CDLToGExpression(left, allNodesMap), CDLToGExpression(right, allNodesMap))
        case GreaterThan(l, r) => GGreaterThan(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case GreaterThanOrEq(l, r) => GGreaterEqThan(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case BtAnd(l, r) => GBtAnd(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case BtOr(l, r) => GBtOr(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case BtXor(l, r) => GBtXor(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case BtLeft(l, r) => GBtLeft(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case BtRight(l, r) => GBtRight(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case Plus(l,r) => GPlus(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case Minus(l,r) => GMinus(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case Dot(l,r) => GDot(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case Times(l,r) => GTimes(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case Div(l,r) => GDivide(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case Mod(l,r) => GMod(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case Not(e) => GNot(CDLToGExpression(e, allNodesMap))
        case FunctionCall("is_substr", List(whole, sub)) => 
            GSubString(CDLToGExpression(whole, allNodesMap), CDLToGExpression(sub, allNodesMap))
        case FunctionCall("bool", List(e)) => GBoolFunc(CDLToGExpression(e, allNodesMap))
        case FunctionCall("is_loaded", List(Identifier(featureName))) =>
          allNodesMap.get(featureName) match {
            case Some(_) => GLongIntLiteral(1)
            case None    => GLongIntLiteral(0)
          }
        case FunctionCall("is_active",  List(Identifier(featureName))) =>
//          GAliasReference(ActiveAliasVariableName(featureName))
          // WARNING: THIS IS A HACK
          // TODO: Replace this with the real semantics whatever that is
          allNodesMap.get(featureName) match {
            case Some(node) => 
              node.flavor match {
                case DataFlavor     => GVariable(DataVariableName(featureName))
                case BoolDataFlavor => GVariable(DataVariableName(featureName))
                case BoolFlavor     => GVariable(BoolVariableName(featureName))
                case NoneFlavor     => GVariable(BoolVariableName(featureName))
              }
            case None          => GLongIntLiteral(0) //TODO: mnovakovic: check this!
          }            

        case FunctionCall("is_enabled", List(Identifier(featureName))) =>
          allNodesMap.get(featureName) match {
            case Some(node) => 
              node.flavor match {
                case BoolFlavor     => GVariable(BoolVariableName(node.id))
                case DataFlavor     => GLongIntLiteral(1)
                case BoolDataFlavor => GVariable(BoolVariableName(node.id))
                case _              => /* None flavor */
                                       GLongIntLiteral(1)
              }
            case None          => GLongIntLiteral(0)
          }
        case _ => throw new Exception("unexpected expression:" + expr)
      }
    }
}


object GExpressionToString {
 def apply(expr:GExpression):String = {
      import gsd.cdl.formula.types._
      expr match {
        case GStringLiteral(v) => "" + v
        case GLongIntLiteral(v) => "" + v
        case GEq(left, right) => GExpressionToString(left) + " == " + GExpressionToString(right)
        case GCast(exp) => "GCast([" + GExpressionToString(exp) + "] -- "  + exp.getType.toString + " to " + exp.getRequiredType.toString + ")"
        case GConditional(cond, pass, fail) => GExpressionToString(cond) + " ? " + GExpressionToString(pass) + " : " + GExpressionToString(fail)
/*
        case GOr(left, right) => GOr(CDLToGExpression(left, allNodesMap), CDLToGExpression(right, allNodesMap))
        case And(left, right) => GAnd(CDLToGExpression(left, allNodesMap), CDLToGExpression(right, allNodesMap))
        case Eq(left, right) => GEq(CDLToGExpression(left, allNodesMap), CDLToGExpression(right, allNodesMap))
        case NEq(left, right) => GNot(GEq(CDLToGExpression(left, allNodesMap), CDLToGExpression(right, allNodesMap)))
        case LessThan(left, right) => GLessThan(CDLToGExpression(left, allNodesMap), CDLToGExpression(right, allNodesMap))
        case LessThanOrEq(left, right) => GLessEqThan(CDLToGExpression(left, allNodesMap), CDLToGExpression(right, allNodesMap))
        case GreaterThan(l, r) => GGreaterThan(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case GreaterThanOrEq(l, r) => GGreaterEqThan(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case BtAnd(l, r) => GBtAnd(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case BtOr(l, r) => GBtOr(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case BtXor(l, r) => GBtXor(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case BtLeft(l, r) => GBtLeft(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case BtRight(l, r) => GBtRight(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case Plus(l,r) => GPlus(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case Minus(l,r) => GMinus(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case Dot(l,r) => GDot(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case Times(l,r) => GTimes(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case Div(l,r) => GDivide(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case Mod(l,r) => GMod(CDLToGExpression(l, allNodesMap), CDLToGExpression(r, allNodesMap))
        case Not(e) => GNot(CDLToGExpression(e, allNodesMap))
        case FunctionCall("is_substr", List(whole, sub)) => 
            GSubString(CDLToGExpression(whole, allNodesMap), CDLToGExpression(sub, allNodesMap))
        case FunctionCall("bool", List(e)) => GBoolFunc(CDLToGExpression(e, allNodesMap))
        case FunctionCall("is_loaded", List(Identifier(featureName))) =>
          allNodesMap.get(featureName) match {
            case Some(_) => GLongIntLiteral(1)
            case None    => GLongIntLiteral(0)
          }
//        case FunctionCall("is_active",  List(Identifier(featureName))) =>
//          GAliasReference(ActiveAliasVariableName(featureName))
//          GVariable(BoolVariableName(featureName))

        case FunctionCall("is_enabled", List(Identifier(featureName))) =>
          }
*/          
//        case e@_ => e.getType().toString() + " - "  + e.getRequiredType().toString() + " - " + e.toString
        case e@_ => e.toString
      }
    }
}

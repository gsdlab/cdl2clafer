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
import gsd.cdl2clafer.model._


object InitialCExpressionTransformation {

 def apply(expr:CExpression, map:scala.collection.immutable.Map[String, Node]):CExpression = {
      expr match {
        case CEq(left, right) => transformEq(left, right, map)
        case CNEq(left, right) => transformNEq(left, right, map)
        case CLessThan(left, right) => transformCLessThan(left, right, map)
        case CLessThanOrEq(left, right) => transformCLessThanOrEq(left, right, map)
        case CGreaterThan(l, r) => transformCGreaterThan(l, r, map)
        case CGreaterThanOrEq(l, r) => transformCGreaterThanOrEq(l, r, map)
//        case Plus(l,r)=>CPlus(transformCExpression(l, map),transformCExpression(r, map))
//        case Minus(l,r)=>CMinus(transformCExpression(l, map),transformCExpression(r, map))
//        case Times(l,r)=>CTimes(transformCExpression(l, map),transformCExpression(r, map))
//        case Div(l,r)=>CDiv(transformCExpression(l, map),transformCExpression(r, map))
        case _ => expr
      }
    }
    
    /**
     * > 
     */
    def transformCGreaterThan(left:CExpression, right:CExpression, map:scala.collection.immutable.Map[String, Node]):CExpression = {
     if (isInterfaceCount(left, right, map)) {
      if (left.isInstanceOf[CIdentifier] && right.isInstanceOf[CLongIntLiteral]) {
       CGreaterThanInterfaceCount(CIdentifier(left.asInstanceOf[CIdentifier].id), 
           CLongIntLiteral(right.asInstanceOf[CLongIntLiteral].value))
      } else if (right.isInstanceOf[CIdentifier] && left.isInstanceOf[CLongIntLiteral]) {
       CGreaterThanInterfaceCount(CIdentifier(right.asInstanceOf[CIdentifier].id), 
           CLongIntLiteral(left.asInstanceOf[CLongIntLiteral].value))
      } else 
       throw new Exception("Strange")
     } else {
      CGreaterThan(InitialCExpressionTransformation(left, map), InitialCExpressionTransformation(right, map))
     }
    }

    /**
     * >=
     */
    def transformCGreaterThanOrEq(left:CExpression, right:CExpression, map:scala.collection.immutable.Map[String, Node]):CExpression = {
     if (isInterfaceCount(left, right, map)) {
      if (left.isInstanceOf[CIdentifier] && right.isInstanceOf[CLongIntLiteral]) {
       CGreaterThanOrEqInterfaceCount(CIdentifier(left.asInstanceOf[CIdentifier].id), 
           CLongIntLiteral(right.asInstanceOf[CLongIntLiteral].value))
      } else if (right.isInstanceOf[CIdentifier] && left.isInstanceOf[CLongIntLiteral]) {
       CGreaterThanOrEqInterfaceCount(CIdentifier(right.asInstanceOf[CIdentifier].id), 
           CLongIntLiteral(left.asInstanceOf[CLongIntLiteral].value))
      } else 
       throw new Exception("Strange")
     } else {
      CGreaterThanOrEq(InitialCExpressionTransformation(left, map), InitialCExpressionTransformation(right, map))
     }
    }

    /**
     * <= 
     */
    def transformCLessThanOrEq(left:CExpression, right:CExpression, map:scala.collection.immutable.Map[String, Node]):CExpression = {
     if (isInterfaceCount(left, right, map)) {
      if (left.isInstanceOf[CIdentifier] && right.isInstanceOf[CLongIntLiteral]) {
       CLessThanOrEqInterfaceCount(CIdentifier(left.asInstanceOf[CIdentifier].id), 
           CLongIntLiteral(right.asInstanceOf[CLongIntLiteral].value))
      } else if (right.isInstanceOf[CIdentifier] && left.isInstanceOf[CLongIntLiteral]) {
       CLessThanOrEqInterfaceCount(CIdentifier(right.asInstanceOf[CIdentifier].id), 
           CLongIntLiteral(left.asInstanceOf[CLongIntLiteral].value))
      } else 
       throw new Exception("Strange")
     } else {
      CLessThanOrEq(InitialCExpressionTransformation(left, map), InitialCExpressionTransformation(right, map))
     }
    }

    /**
     * <
     */
    def transformCLessThan(left:CExpression, right:CExpression, map:scala.collection.immutable.Map[String, Node]):CExpression = {
     if (isInterfaceCount(left, right, map)) {
      if (left.isInstanceOf[CIdentifier] && right.isInstanceOf[CLongIntLiteral]) {
       CLessThanInterfaceCount(CIdentifier(left.asInstanceOf[CIdentifier].id), 
           CLongIntLiteral(right.asInstanceOf[CLongIntLiteral].value))
      } else if (right.isInstanceOf[CIdentifier] && left.isInstanceOf[CLongIntLiteral]) {
       CLessThanInterfaceCount(CIdentifier(right.asInstanceOf[CIdentifier].id), 
           CLongIntLiteral(left.asInstanceOf[CLongIntLiteral].value))
      } else 
       throw new Exception("Strange")
     } else {
      CLessThan(InitialCExpressionTransformation(left, map), InitialCExpressionTransformation(right, map))
     }
    }

    /**
     * ==
     */
    def transformEq(left:CExpression, right:CExpression, map:scala.collection.immutable.Map[String, Node]):CExpression = {
     if (isInterfaceCount(left, right, map)) {
      if (left.isInstanceOf[CIdentifier] && right.isInstanceOf[CLongIntLiteral]) {
       CEqInterfaceCount(CIdentifier(left.asInstanceOf[CIdentifier].id), 
           CLongIntLiteral(right.asInstanceOf[CLongIntLiteral].value))
      } else if (right.isInstanceOf[CIdentifier] && left.isInstanceOf[CLongIntLiteral]) {
       CEqInterfaceCount(CIdentifier(right.asInstanceOf[CIdentifier].id), 
           CLongIntLiteral(left.asInstanceOf[CLongIntLiteral].value))
      } else 
       throw new Exception("Strange")
     } else {
      CEq(InitialCExpressionTransformation(left, map), InitialCExpressionTransformation(right, map))
     }
    }


    /**
     * !=
     */
    def transformNEq(left:CExpression, right:CExpression, map:scala.collection.immutable.Map[String, Node]):CExpression = {
     if (isInterfaceCount(left, right, map)) {
      if (left.isInstanceOf[CIdentifier] && right.isInstanceOf[CLongIntLiteral]) {
       CNEqInterfaceCount(CIdentifier(left.asInstanceOf[CIdentifier].id), 
           CLongIntLiteral(right.asInstanceOf[CLongIntLiteral].value))
      } else if (right.isInstanceOf[CIdentifier] && left.isInstanceOf[CLongIntLiteral]) {
       CNEqInterfaceCount(CIdentifier(right.asInstanceOf[CIdentifier].id), 
           CLongIntLiteral(left.asInstanceOf[CLongIntLiteral].value))
      } else 
       throw new Exception("Strange")
     } else {
      CNEq(InitialCExpressionTransformation(left, map), InitialCExpressionTransformation(right, map))
     }
    }

    /**
     * Tests whether this expression is related to counting of
     * interfaces, for exampple A == 1 is checking whether
     * the number of instances of interface A is equal to 1.
     * 
     * This greatly simplifies the conversion to Clafer since Clafer
     * treats this kind of thing differently than regular comparison
     * In other words, A == 1 is treated in different manner if A is/is not
     * an interface
     */
    def isInterfaceCount(left:CExpression, right:CExpression, map:scala.collection.immutable.Map[String, Node]):Boolean = {
     if (left.isInstanceOf[CIdentifier] && right.isInstanceOf[CLongIntLiteral]) {
      val id = left.asInstanceOf[CIdentifier].id
      if (map.contains(id) && map.apply(id).cdlType == InterfaceType) {
       true
      } else {
       false
      }
     } else if (right.isInstanceOf[CIdentifier] && left.isInstanceOf[CLongIntLiteral]) {
      val id = right.asInstanceOf[CIdentifier].id
      if (map.contains(id) && map.apply(id).cdlType == InterfaceType) {
       true
      } else {
       false
      }
     } else {
      false
     }
    }
}

/*
 * Copyright (c) 2010 Marko Novakovic <mnovakov@gsd.uwaterloo.ca>
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
import gsd.cdl.formula.types._

case object UndefinedType extends Type {
  override def toString = "Undefined"
}

case object DisjunctiveType extends Type {
  override def toString = "Disjunctive"
}

case class GTrue() extends GExpression {
  override def toString = "True"
}

case class GFalse() extends GExpression {
  override def toString = "True"
}

case object ArtificialInterfaceVariableName 
	extends VariableName("_INTERFACE_ARTIFICIALLY_ADDED")

case object AbstractVariableName 
	extends VariableName("_ABSTRACT")


case class GClaferNoInstances(exp:GExpression) extends GExpression
case class GImplies(left:GExpression, right:GExpression) extends GBinaryExpression(left, right, "=>")
case class GEquivalent(left:GExpression, right:GExpression) extends GBinaryExpression(left, right, "<=>")
case class GNumImplementations(exp:GVariable) extends GExpression


//case object ArtificialDataInterfaceVariableName 
//	extends VariableName("_DATA_INTERFACE_ARTIFICIALLY_ADDED")
//
//case object ArtificialBoolDataInterfaceVariableName 
//	extends VariableName("_BOOLDATA_INTERFACE_ARTIFICIALLY_ADDED")
//
//case object ArtificialBoolInterfaceVariableName 
//	extends VariableName("_BOOL_INTERFACE_ARTIFICIALLY_ADDED")


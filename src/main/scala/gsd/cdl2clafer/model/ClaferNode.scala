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

import gsd.cdl2clafer._
import gsd.cdl.formula._
import gsd.cdl.model._

case class ClaferNode(
                name : String, 
                isMandatory: Boolean,
                isAbstract: Boolean,
                claferType : types.Type, // is string enough?
                display : String,
                description : Option[String],
                constraints : List[GExpression],
                legalValues : Option[GExpression],
//                defaultValue : Option[GExpression], //TODO restrict to only literals
                implements : List[GExpression], // just identifiers
                children : List[ClaferNode])

// determine Enumerations 
// determine Interfaces
// determine types
// for every Node
//Create ClaferNode
//set type
//set isOptional
//set display
//set description
//foreach active_if and requires exp
//constraints.add(transsofmCDLExpression(exp))
//add implements
//foreach child do the same


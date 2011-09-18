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
 
package gsd.cdl2clafer.model

//import gsd.cdl2clafer._
//import gsd.cdl.formula._
//import gsd.cdl.model._

/**
 *  
 **/
abstract class CDLType

case class CDLPackageType() extends CDLType
case class CDLComponentType() extends CDLType
case class CDLOptionType() extends CDLType
case class CDLInterfaceType() extends CDLType

/**
 * Artificial types 
 **/
case class ArtificialBooleanType() extends CDLType
case class ArtificialInterfaceType() extends CDLType

/**
 * Do not display as a comment
 **/
case class OtherType() extends CDLType
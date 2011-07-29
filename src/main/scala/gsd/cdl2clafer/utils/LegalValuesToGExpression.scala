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

object LegalValuesToGExpression {

  def apply(n:Node, allNodesMap:scala.collection.immutable.Map[String, Node]):Option[GExpression] = {
    if (n.legalValues == None) { 
      None
    } else {
      val legalValues = n.legalValues.getOrElse(null)
//      legalValues.ranges.foreach(println)
      val constraint = legalValues.ranges.map( _ match {
                                    case MinMaxRange(low, high) => { 
                                    	(GGreaterEqThan(GVariable(DataVariableName(n.id)), CDLToGExpression(low, allNodesMap))) & 
                                    		(GVariable(DataVariableName(n.id)) <= CDLToGExpression(high, allNodesMap))
                                    	
                                    }
                                    case SingleValueRange(v) => {
                                          GVariable(DataVariableName(n.id)) === CDLToGExpression(v, allNodesMap)
                                    }
                            }).reduceLeft(_ | _)

      Some(constraint)
    }
  }
}

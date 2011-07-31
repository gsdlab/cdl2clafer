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
import org.kiama.rewriting.Rewriter._

object CDL2ClaferRun {

  def main( args: Array[String] ){
   if (args.size == 0) {
    println("Not enough arguments")
   } else {
    if (args(0) == "--t") {
     if (args.size == 2) {
      Converter.printIMLAsClafer(args(1))
     } else {
  		  val file = "input/extracted_representative_model-cleaned.iml"
  		  Converter.printIMLAsClafer(file)     
  		 }
    } else if (args(0) == "-c") {
      TestTypes.testCollectl
    } else if (args(0) == "--query") {
      QueryAllFeaturesInDir()
    } else if (args(0) == "--tc") {
      TestTypes.testCounting()
    } else if (args(0) == "--convert-all") {
        TestTypes.all()    
    } else if (args(0) == "--convert-all-with-output") {
        TestTypes.writeAll()    
    }
   }
  }
}

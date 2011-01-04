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

object CDL2ClaferRun {
  def main( args: Array[String] ){
//    gsd.cdl2clafer.CDL2Clafer.processCDLFile("problems.iml", "problems.cfr.txt")
    gsd.cdl2clafer.CDL2Clafer.processCDLFile("pc_vmWare.iml.txt", "pc_vmWare.cfr.txt")
  }
}
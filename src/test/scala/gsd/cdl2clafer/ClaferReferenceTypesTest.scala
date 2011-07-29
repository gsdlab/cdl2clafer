package gsd.cdl2clafer

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

import scala.collection.mutable

import org.scalatest.FunSuite
/*
import gsd.cdl2clafer.CDL2Clafer.EnumRef

class ClaferReferenceTypesTest extends FunSuite {

  test("Test Enums") {
    var enum = new EnumRef("ENUM_TEST1")
    enum.addEnumElement("14D")
    enum.addEnumElement("14")
    enum.addEnumElement("DD14")
    enum.addEnumElement("DD14\"")
    enum.getEnumElementsPrepared.foreach(println)
//    enum.getEnumElementsKeys.foreach(println)

//    println(enum.getEnumElementByKey("14").asInstanceOf[Some[String]].get)
    assert(enum.getEnumElementByKey("14").isInstanceOf[Some[String]])
    assert("VAL_14" == enum.getEnumElementByKey("14").asInstanceOf[Some[String]].get)
  }
}
*/

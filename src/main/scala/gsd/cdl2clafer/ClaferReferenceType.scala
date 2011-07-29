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

abstract class ClaferReferenceType

case class IntegerRef() extends ClaferReferenceType {
  override def toString = "integer"
}

case class StringRef() extends ClaferReferenceType {
  override def toString = "string"
}

case class EnumRef() extends ClaferReferenceType {
}

case class EnumRef1(nodeName: String) extends ClaferReferenceType {
  private var enumMap = Map[String, String]()

  override def toString = nodeName

  def toClaferRepresentation: String = {
    ""
  }

  private def guardString(s: String): String = {
    s.trim.replaceAll("\"", "")
  }

  def addEnumElement(enum: String) = {
    val newEnumName = guardString(enum)
    try {
      val intEnumVal = Integer.parseInt(newEnumName)
      enumMap += (enum -> ("VAL_" + intEnumVal + ""))
    } catch {
      case nfe: NumberFormatException => {
        enumMap += (enum -> newEnumName)
      }
    }
  }

  def getEnumElementByKey(key: String): Option[String] = {
    enumMap.get(key.trim)
  }

  def getEnumElementsPrepared(): List[String] = {
    enumMap.values.toList // return copy?
  }

  //    def getEnumElementsKeys() : List[String] = {
  //      enumMap.keys.toList // return copy?
  //    }
}

case class NoRef() extends ClaferReferenceType {
  override def toString = "no ref"
}

case class UnknownRef() extends ClaferReferenceType {
  override def toString = "unknown"
}


case class BooleanRef() extends ClaferReferenceType {
  override def toString = "boolean"
}

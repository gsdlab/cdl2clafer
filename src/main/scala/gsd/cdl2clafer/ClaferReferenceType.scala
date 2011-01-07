package gsd.cdl2clafer

abstract class ClaferReferenceType

case class IntegerRef extends ClaferReferenceType {
  override def toString = "integer"
}

case class StringRef extends ClaferReferenceType {
  override def toString = "string"
}

case class EnumRef(nodeName: String) extends ClaferReferenceType {
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

case class NoRef extends ClaferReferenceType {
  override def toString = "unknown"
}

case class BooleanRef extends ClaferReferenceType

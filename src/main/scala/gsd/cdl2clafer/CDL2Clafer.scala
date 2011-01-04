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
import util.parsing.input.PagedSeqReader
import collection.immutable.PagedSeq
import kiama.rewriting.Rewriter
import scala.collection.jcl.Conversions._
import java.io.FileWriter
import gsd.cdl.{CDLExpression, IMLParser}

object CDL2Clafer extends IMLParser with Rewriter {

  var nodesById = Map[String, Node]()
  var childParentMap = Map[String,String]()
//
//  def main( args: Array[String] ){
////    processCDLFile("problems.iml", "pc_vmWare.iml.txt")
//    processCDLFile("pc_vmWare.iml.txt", "pc_vmWare.iml.txt")
//  }

  def processCDLFile (inputFile: String, outputFile: String) {
    parseAll(cdl, new PagedSeqReader(PagedSeq fromFile getBaseInputDir + inputFile)) match{
      case Success(res,_) => {
        val claferString = asClaferString( res )
        println(claferString)
        printToFile(claferString, outputFile)
      }
      case x => println( "failure: " + x )
    }
  }

  private def getBaseInputDir = {
      System.getProperty("user.dir") + "/ecos/input/"
  }

  private def getBaseOutputDir = {
      System.getProperty("user.dir") + "/ecos/output/"
  }

  private def indent (count: Int, builder : StringBuilder) : StringBuilder = {
    builder.append(indent(count))
  }

  private def indent(count: Int) : String = {
    val builder = new StringBuilder
    for {i <- 0 to count} builder.append(indent)
    builder.toString
  }

  private def newLineAndIndent(count: Int) : String = {
    newLine + indent(count)
  }

  private def newLine () = "\n"
  private def indent () = "    "

  private def appendActiveIfs (n: Node, depth: Int, builder : StringBuilder) = {
    n.activeIfs.foreach(activeIf => builder.append(newLineAndIndent(depth)).
      append("-- ifActive").
//      .append(" (").append(activeIf.getClass).append(")").     //TODO: Remove this line
      append(newLine).append(indent(depth)).
      append("[").append(getCDLExpressionAsString(activeIf)).append("]")
      )
  }

  private def appendImplements (n: Node, depth: Int, builder : StringBuilder) = {
    if (n.implements.size > 1)
      n.implements.foreach(implement =>
          builder.append(newLine).append(indent(depth)).
          append("'").append(getCDLExpressionAsString(implement))
        )
  }

  private def appendReqs (n: Node, refType: ClaferReferenceType, depth: Int, builder : StringBuilder) = {
    n.reqs.foreach(req =>
        builder.append(newLine).append(indent(depth)).
        append("[").append(getCDLExpressionAsString(req)).append("]")
      )
  }

  private def appendFirstLineOfClafer(builder: StringBuilder, depth: Int, reference: ClaferReferenceType, n: Node) = {
    builder.append(newLineAndIndent(depth - 1))
    if (n.cdlType == InterfaceType && n.flavor == DataFlavor) {
      builder.append("abstract ")
    }
    builder.append(n.id) // Clafer name
    if (n.implements.size == 1)
      builder.append(" extends ").append(getCDLExpressionAsString(n.implements.first))

    if (!reference.isInstanceOf[NoRef] && !reference.isInstanceOf[BooleanRef]) {
      builder.append(" -> ").append(reference).append(" ")
    }

    n.cdlType match {
      case OptionType => {
        if (n.flavor == BoolFlavor)
          builder.append(" ?")
      }
      case _ => {}
    }
  }

  private def getCalculatedxpressionAsString (e : CDLExpression, level: Int, depth: Int) : String = {
    e match {
      case StringLiteral(value) => {
//        if (value.substring(0, 1) == "\"" && value.substring(value.length - 1, value.length) == "\"") {
//          value.substring(1, value.length - 1)
//        }  else {
          value
//        }
      }
      case Dot(left, right) => {
        val builder = new StringBuilder
        builder.
          append("(").
          append(getCalculatedxpressionAsString(left, level, depth)).
          append(")").
          append(" + ").
          append(newLineAndIndent(level + 2 + depth)).
          append("(").
          append(getCalculatedxpressionAsString(right, level, depth)).
          append(")").toString
      }
      case Conditional(cond, pass, fail) => {
        val builder = new StringBuilder
//        builder.append(indent(depth + 0))
        if (!fail.isInstanceOf[Conditional] && !pass.isInstanceOf[Conditional]) {
          if (level != 0) {
            builder.append("(")
          }
          builder.
          append(getCalculatedxpressionAsString(cond, level + 1, depth)).
          append(" => ").
          append(getCalculatedxpressionAsString(pass, level + 1, depth)).
          append(" else ").
          append(getCalculatedxpressionAsString(fail, level + 1, depth))
          if (level != 0) {
            builder.append(")")
          }
        } else {
          builder.append(indent(depth + 1)).
          append(getCalculatedxpressionAsString(cond, level + 1, depth)).
          append(" => ").append(getCalculatedxpressionAsString(pass, level + 1, depth)).
          append(newLineAndIndent(depth + level + 2)).
          append("else ").
          append(newLineAndIndent(depth + level)).
          append(getCalculatedxpressionAsString(fail, level + 1, depth))
        }
        builder.append("")

        builder.toString
      }
      case _ => {getCDLExpressionAsString(e)}
    }

  }

  private def getCalculatedxpressionAsString (e : CDLExpression, depth: Int) : String = {
    getCalculatedxpressionAsString(e, 0, depth)
  }

  private def getCDLExpressionAsStringWithType(e: CDLExpression, refType: ClaferReferenceType): String = {
    e match {
      case Identifier(s) => {
        if (refType.isInstanceOf[IntegerRef]) {
          "#" + getCDLExpressionAsString(e)
        } else {
          getCDLExpressionAsString(e)
        }
      }
      case Plus(first, second) => {
        getCDLExpressionAsStringWithType(first, refType) + " + " + getCDLExpressionAsStringWithType(second, refType)
      }

      case _ => {getCDLExpressionAsString(e)}
    }
  }

  abstract class ClaferReferenceType
  case class IntegerRef extends ClaferReferenceType {
    override def toString = "integer"
  }
  case class StringRef extends ClaferReferenceType {
    override def toString = "string"
  }
  case class EnumRef(nodeName: String) extends ClaferReferenceType {
    private var enumList = List[String]()
    override def toString = nodeName

    def addEnumElement(enum: String) = {
      enumList += enum
    }

    def getEnumElements() :List[String] = {
      enumList.toList // return copy?
    }
  }
  case class NoRef extends ClaferReferenceType {
    override def toString = "unknown"
  }
  case class BooleanRef extends ClaferReferenceType

  /**
   *
   * Converts CDLExpression to String
   * TODO: Refactor! Should be much smaller method
   *
   * */
  private def getCDLExpressionAsString (e : CDLExpression) : String = {
    e match {
      case StringLiteral(value) => {
//        if (value.substring(0, 1) == "\"" && value.substring(value.length - 1, value.length) == "\"") {
//          value.substring(1, value.length - 1)
//        }  else {
          value
//        }
      }
      case IntLiteral(value) => {String.valueOf(value)}
      case Eq(left, right) => {
        if (left.isInstanceOf[Identifier] && getCDLExpressionType(right).isInstanceOf[IntegerRef]) {
          "#" + getCDLExpressionAsString(left) + " = " + getCDLExpressionAsString(right)
        } else if (getCDLExpressionType(left).isInstanceOf[IntegerRef] && right.isInstanceOf[Identifier]) {
          getCDLExpressionAsString(left) + " = #" + getCDLExpressionAsString(right)
        } else {
          getCDLExpressionAsString(left) + " = " + getCDLExpressionAsString(right)
        }
      }
      case NEq(left, right) => {
        if (left.isInstanceOf[Identifier] && getCDLExpressionType(right).isInstanceOf[IntegerRef]) {
          "#" + getCDLExpressionAsString(left) + " != " + getCDLExpressionAsString(right)
        } else if (getCDLExpressionType(left).isInstanceOf[IntegerRef] && right.isInstanceOf[Identifier]) {
          getCDLExpressionAsString(left) + " != #" + getCDLExpressionAsString(right)
        } else {
          getCDLExpressionAsString(left) + " != " + getCDLExpressionAsString(right)
        }
      }
      case GreaterThanOrEq(left, right) => {
        if (left.isInstanceOf[Identifier] && getCDLExpressionType(right).isInstanceOf[IntegerRef]) {
          "#" + getCDLExpressionAsString(left) + " >= " + getCDLExpressionAsString(right)
        } else if (getCDLExpressionType(left).isInstanceOf[IntegerRef] && right.isInstanceOf[Identifier]) {
          getCDLExpressionAsString(left) + " >= #" + getCDLExpressionAsString(right)
        } else {
          getCDLExpressionAsString(left) + " >= " + getCDLExpressionAsString(right)
        }
      }
      case GreaterThan(left, right) => {
        if (left.isInstanceOf[Identifier] && getCDLExpressionType(right).isInstanceOf[IntegerRef]) {
          "#" + getCDLExpressionAsString(left) + " > " + getCDLExpressionAsString(right)
        } else if (getCDLExpressionType(left).isInstanceOf[IntegerRef] && right.isInstanceOf[Identifier]) {
          getCDLExpressionAsString(left) + " > #" + getCDLExpressionAsString(right)
        } else {
          getCDLExpressionAsString(left) + " > " + getCDLExpressionAsString(right)
        }
      }
      case LessThanOrEq(left, right) => {
        if (left.isInstanceOf[Identifier] && getCDLExpressionType(right).isInstanceOf[IntegerRef]) {
          "#" + getCDLExpressionAsString(left) + " <= " + getCDLExpressionAsString(right)
        } else if (getCDLExpressionType(left).isInstanceOf[IntegerRef] && right.isInstanceOf[Identifier]) {
          getCDLExpressionAsString(left) + " <= #" + getCDLExpressionAsString(right)
        } else {
          getCDLExpressionAsString(left) + " <= " + getCDLExpressionAsString(right)
        }
      }
      case LessThan(left, right) => {
        if (left.isInstanceOf[Identifier] && getCDLExpressionType(right).isInstanceOf[IntegerRef]) {
          "#" + getCDLExpressionAsString(left) + " < " + getCDLExpressionAsString(right)
        } else if (getCDLExpressionType(left).isInstanceOf[IntegerRef] && right.isInstanceOf[Identifier]) {
          getCDLExpressionAsString(left) + " < #" + getCDLExpressionAsString(right)
        } else {
          getCDLExpressionAsString(left) + " < " + getCDLExpressionAsString(right)
        }
      }
      case Identifier(s) => {s}
      case Not(s) => {"!" + s}
      case Plus(first, second) => {
        if (getCDLExpressionType(first).isInstanceOf[IntegerRef] || getCDLExpressionType(second).isInstanceOf[IntegerRef]) {
          if (first.isInstanceOf[Identifier]) {
            "#" + getCDLExpressionAsString(first) + " + " + getCDLExpressionAsString(second)
          } else if (second.isInstanceOf[Identifier]) {
            getCDLExpressionAsString(first) + " + #" + getCDLExpressionAsString(second)
          } else {
            getCDLExpressionAsString(first) + " + " + getCDLExpressionAsString(second)
          }
        } else {
            "(" +
            getCDLExpressionAsString(first) +
            " + " +
            getCDLExpressionAsString(second) +
            ")"
        }
      }
      case Minus(first, second) => {
        if (getCDLExpressionType(first).isInstanceOf[IntegerRef] || getCDLExpressionType(second).isInstanceOf[IntegerRef]) {
          if (first.isInstanceOf[Identifier]) {
            "#" + getCDLExpressionAsString(first) + " - " + getCDLExpressionAsString(second)
          } else if (second.isInstanceOf[Identifier]) {
            getCDLExpressionAsString(first) + " - #" + getCDLExpressionAsString(second)
          } else {
            getCDLExpressionAsString(first) + " - " + getCDLExpressionAsString(second)
          }
        } else {
            "(" +
            getCDLExpressionAsString(first) +
            " - " +
            getCDLExpressionAsString(second) +
            ")"
        }
      }
      case Or(left, right) => {getCDLExpressionAsString(left) + " || " + getCDLExpressionAsString(right) }
      case And(left, right) => {getCDLExpressionAsString(left) + " && " + getCDLExpressionAsString(right) }
      case Conditional(cond, pass, fail: CDLExpression) => {
        val builder = new StringBuilder
        builder.
          append("((").
          append(getCDLExpressionAsString(cond)).
          append(")").
          append(" => ").
          append(getCDLExpressionAsString(pass)).
          append(" else ").
          append(getCDLExpressionAsString(fail)).
          append(")")
        builder.toString
      }
      case _ => {e.toString}
    }
  }

  private def appendDescription(n: Node, builder: StringBuilder, depth: Int): Unit = {
    n.description match {
      case Some(value) => {
        builder.append(newLineAndIndent(depth)).append("description = " + value)
      }
      case None => {}
    }
  }

  private def appendDisplay(builder: StringBuilder, depth: Int, n: Node) = {
    if (!n.display.isEmpty) {
      builder.append(newLineAndIndent(depth)).append("display = " + n.display.toString)
    }
  }

  private def appendDefaultValues(n: Node, refType: ClaferReferenceType, builder: StringBuilder, depth: Int) = {
    n.defaultValue match {
      case Some(value) => {
        builder.
          append(newLineAndIndent(depth)).
          append("-- default_value = ").
          append(getCDLExpressionAsString(value))
//          append(getCDLExpressionAsStringWithType(value, refType))
      }
      case _ => {}
    }
  }

  private def getEnumNameForNode(n: Node): String = {
    "" + n.id + "_ENUM"
  }

  private def appendEnumDeclaration(enum: ClaferReferenceType, builder: StringBuilder, n: Node): Unit = {
    if (enum.isInstanceOf[EnumRef]) {

      builder.append(newLine).append("enum ").append(enum.asInstanceOf[EnumRef].nodeName).append(" = ")
      var these = enum.asInstanceOf[EnumRef].getEnumElements
      for {i <- 0 to enum.asInstanceOf[EnumRef].getEnumElements.size - 1} {
        builder.append(these.head)
        these = these.tail
        if (i != enum.asInstanceOf[EnumRef].getEnumElements.size - 1)
          builder.append(" | ")
      }
    }
  }

  /**
  *
  **/
  private def appendLegalValuesDeclaration(n: Node, refType: ClaferReferenceType, builder: StringBuilder, depth: Int) = {
    n.legalValues match {
      case Some(legalValueOption: LegalValuesOption) => {
        legalValueOption.ranges.foreach(
          range_ => range_ match {
            case MinMaxRange(low, high) => {
              builder.
                append(newLineAndIndent(depth)).
                append("[").
                //                  append(getCDLExpressionAsStringWithType(low, refType)).
                append(getCDLExpressionAsString(low)).
                append(" <= this && this <= ").
                append(getCDLExpressionAsString(high)).
                //                  append(getCDLExpressionAsStringWithType(high, refType)).
                append("]")
            }

            case SingleValueRange(r) => {}
          }
        )
      }

      case None => {}
    }
  }

//  private def getTypeOfExpression(e: CDLExpression) : ClaferReferenceType = {
//      e match {
//        case Eq(left, right) => {
//          if (left.isInstanceOf[IntLiteral] || left.isInstanceOf[IntLiteral])
//            new IntegerRef
//          else
//            new NoRef
//        }
//        case NEq(left, right) => {
//          if (left.isInstanceOf[IntLiteral] || left.isInstanceOf[IntLiteral])
//            new IntegerRef
//          else
//            new NoRef
//        }
//        case _ => {new NoRef}
//      }
//  }

//  private def getCDLExpressionType(expression : CDLExpression) : ClaferReferenceType = {
//    expression match {
//      case IntLiteral(s) => {
//        new IntegerRef
//      }
//      case StringLiteral(value) => {
//        if (value.substring(1, 2) == "x") {
//          new IntegerRef
//        } else {
//          new StringRef
//        }
//      }
//      case Plus (left, right) => {
//        new IntegerRef
//      }
//      case _ => new NoRef
//    }
//  }

  private def getCDLExpressionType(expression : CDLExpression) : ClaferReferenceType = {
    expression match {
      case Conditional(cond, pass, fail) => {
          getCDLExpressionType(pass)
      }
      case IntLiteral(s) => {
        new IntegerRef
      }
      case StringLiteral(value) => {
        if (value.substring(1, 2) == "x") {
          new IntegerRef
        } else {
          new StringRef
        }
      }
      case Plus(left, right) => {new IntegerRef} // check
      case Minus(left, right) => {new IntegerRef} // check
      case _ => new NoRef //"unknown (" + expression.getClass.toString + ")"  // error! TODO: cover all cases
    }
  }

  private def appendCalculated(n: Node, builder: StringBuilder, depth: Int): Unit = {
    n.calculated match {
      case Some(expr) => {
        builder.
          append(newLineAndIndent(depth)).
          append("calculated: [this = ").
          append(newLineAndIndent(depth)).
          append(getCalculatedxpressionAsString(expr, depth)).
          append("]")
      }
      case None =>
    }
  }

  private def getClaferTypeFromLegalValues(n: Node): ClaferReferenceType = {
    n.legalValues match {
      case Some(legalValueOption: LegalValuesOption) => {
        if (!legalValueOption.ranges.isEmpty) {
          legalValueOption.ranges.first match {
            case SingleValueRange(value) => {
              val enumRef = new EnumRef(getEnumNameForNode(n))
              legalValueOption.ranges.foreach(
              range_ => range_ match {
                case SingleValueRange(value) => {
                  enumRef.addEnumElement(getCDLExpressionAsString(value))
                }
              })
              enumRef
            }
            case MinMaxRange(low, high) => {
              if (getCDLExpressionType(low).isInstanceOf[IntegerRef]
                || getCDLExpressionType(high).isInstanceOf[IntegerRef]) {
                new IntegerRef
              } else {
                new NoRef
              }
            }
          }
        } else {
          new NoRef
        }
      }

      case None => {new NoRef}
    }
  }

  /**
   * Gets reference type for clafer. i.e integer, enum, string
   * The process goes as follows:
   * 1. See if there is Calculated value in this node.
   *    Return type of leaf elements from calculated
   * 2. Take Legal Values from node.
   *    If there is a list of SingleValues, reference is Enum
   *    If there is MinMax value, read both and see if it is Integer Type
   * 3. If Legal Values returns NoRef, proceed to default values and try
   *    to figure out type from there.
   *
  **/
  private def getClaferType(n: Node): ClaferReferenceType = {

    if (n.flavor != BoolFlavor) {
      n.calculated match {
        case Some(calculated: CDLExpression) => {
          if (calculated.isInstanceOf[Conditional]) {
            // TODO: refactor
            // this might be optimized,
            // we are traversing through the tree of CDLExpressions
            // and are looking for leaf nodes.
            var leafNodes = List[CDLExpression]()
            collectl {
              case e:CDLExpression => {
                if (e.isInstanceOf[Conditional]) {
                  if (!e.asInstanceOf[Conditional].pass.isInstanceOf[Conditional]) {
                    leafNodes += e.asInstanceOf[Conditional].pass
                  } else if (!e.asInstanceOf[Conditional].fail.isInstanceOf[Conditional]) {
                    leafNodes += e.asInstanceOf[Conditional].fail
                  }
                }
              }
            }(calculated)

            /**
             * assuming leafNodes contains final elements of calculated
             * and they are all of the same type,
             * we are returning the type of first of them
             *
             * TODO: should this be represented as enum?
             **/
            getCDLExpressionType(leafNodes.apply(0))
          } else {
            getCDLExpressionType(calculated)
          }
        }
        case None => {
          val referenceTypeFromLegalValues = getClaferTypeFromLegalValues(n)
          if (!referenceTypeFromLegalValues.isInstanceOf[NoRef]) {
            referenceTypeFromLegalValues
          } else {
            n.defaultValue match {
              case Some(expression) => {
                getCDLExpressionType(expression)
              }
              case None => {
                new NoRef
              }
            }
          }

        }
      }

    } else {
      new BooleanRef
    }
  }

  /**
  *  Main method that converts Node to clafer string
  **/
  private def cdlNodeToClaferString(n : Node, depth : Int) : String = {
    val builder = new StringBuilder
    builder.append(newLine)

    val refType = getClaferType(n)

    appendFirstLineOfClafer(builder, depth, refType, n)
    appendDisplay(builder, depth, n)
    appendImplements(n, depth, builder)
    appendDescription(n, builder, depth)
    appendDefaultValues(n, refType, builder, depth)
    appendLegalValuesDeclaration(n, refType, builder, depth)
    appendActiveIfs(n, depth, builder)
    appendReqs(n, refType, depth, builder)
    appendCalculated(n, builder, depth)
    appendEnumDeclaration(refType, builder, n)

//    builder.append(newLineAndIndent(depth)).append("FLAVOR: ").append(n.flavor)

    //recursively print children
    n.children.foreach(child => builder.append(cdlNodeToClaferString(child, depth + 1)))

    builder.toString
  }

  private def printToFile(text: String, outputFile: String): Unit = {
    val file = getBaseOutputDir + outputFile
    val fw = new FileWriter(file)
    try {
      fw.write(text)
    } catch {
      case e: Exception => println(e)
    }
    finally {
      fw.close
    }
  }

  /**
   *
   **/
  def asClaferString( topLevelNodes:List[Node] ) : String = {
    val builder = new StringBuilder
    topLevelNodes.foreach(node => builder.append(cdlNodeToClaferString(node, 0)))

    builder.toString
  }

  /**
   * Reads IML file from inputFile
   * and returns Clafer representation
   **/
  def getClaferStringFromIMLFile( inputFile: String ) : String = {
    parseAll(cdl, new PagedSeqReader(PagedSeq fromFile inputFile)) match {
      case Success(res,_) => {
        asClaferString( res )
      }
      case x => ""
    }
  }
}
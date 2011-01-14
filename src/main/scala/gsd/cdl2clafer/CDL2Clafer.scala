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
import gsd.cdl.IMLParser

object CDL2Clafer extends IMLParser with Rewriter {

  var nodesById = Map[String, Node]()
  var childParentMap = Map[String,String]()

  def processIMLFromFile (inputFile: String, outputFile: String) {
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
  private def concatenation () = "++"

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

  private def isCalculatedExpressionBoolean (expression : CDLExpression) : Boolean = {
    expression match {
      case Eq(l, r) => {true}
      case NEq(l, r) => {true}
      case Not(e) => {true}
      case Identifier(s) => {true}
      case Or(l, r) => {true}
      case And(l, r) => {true}
      case _ => {false}
    }
  }

  private def appendReqs (n: Node, depth: Int, builder : StringBuilder) = {
    n.reqs.foreach(req =>
        builder.append(newLine).append(indent(depth)).
        append("[").append(getCDLExpressionAsString(req)).append("]")
      )
  }

  private def appendFirstLineOfClafer(builder: StringBuilder, depth: Int, n: Node) = {
    builder.append(newLineAndIndent(depth - 1))
    if (n.cdlType == InterfaceType && n.flavor == DataFlavor) {
      builder.append("abstract ")
    }
    builder.append(n.id) // ClaferReferenceHolder name
    if (n.implements.size == 1)
      builder.append(" extends ").append(getCDLExpressionAsString(n.implements.first))

    if (!referenceType.isInstanceOf[NoRef] && !referenceType.isInstanceOf[BooleanRef]) {
      builder.append(" -> ").append(referenceType).append(" ")
    }

    if (isOptional(n)) {
        builder.append(" ?")
    }
  }

  /**
   * TODO: check if this the regular way of setting a ClaferReferenceHolder as optional
   * For now, options and components that are either of Boolean or BooleanData flavor
   * are optional ONLY if calculated is not - 1
   */
  private def isOptional(n: Node) : Boolean = {
    return isOptionalByNature(n) && !isMandatoryByCalculated(n)
  }

  /**
   * Options and Components that are either of Boolean or BooleanData flavor
   * are optional
   */
  private def isOptionalByNature(n: Node) : Boolean = {
    if ((n.cdlType == OptionType || n.cdlType == ComponentType) && (n.flavor == BoolFlavor || n.flavor == BoolDataFlavor)) {
        true
    } else {
        false
    }
  }

  /**
   * Is Calculated value 1?
   */
  private def isMandatoryByCalculated(n: Node) : Boolean = {
    n.calculated match {
      case Some(expr) => {
        (expr.isInstanceOf[IntLiteral] && expr.asInstanceOf[IntLiteral].value == 1)
      }
      case None => {false}
    }
  }

  private def getCalculatedExpressionAsString (e : CDLExpression, level: Int, depth: Int) : String = {
    e match {
      case Dot(left, right) => {
        val builder = new StringBuilder
        builder.
          append("(").
          append(getCalculatedExpressionAsString(left, level, depth)).
          append(") ").
          append(concatenation).
          append(newLineAndIndent(level + 2 + depth)).
          append("(").
          append(getCalculatedExpressionAsString(right, level, depth)).
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
          append(getCalculatedExpressionAsString(cond, level + 1, depth)).
          append(" => ").
          append(getCalculatedExpressionAsString(pass, level + 1, depth)).
          append(" else ").
          append(getCalculatedExpressionAsString(fail, level + 1, depth))
          if (level != 0) {
            builder.append(")")
          }
        } else {
          builder.append(indent(depth + 1)).
          append(getCalculatedExpressionAsString(cond, level + 1, depth)).
          append(" => ").append(getCalculatedExpressionAsString(pass, level + 1, depth)).
          append(newLineAndIndent(depth + level + 2)).
          append("else ").
          append(newLineAndIndent(depth + level)).
          append(getCalculatedExpressionAsString(fail, level + 1, depth))
        }
        builder.append("").toString
      }
      case _ => {getCDLExpressionAsString(e)}
    }

  }

  private def getCalculatedExpressionAsString (e : CDLExpression, depth: Int) : String = {
    getCalculatedExpressionAsString(e, 0, depth)
  }

  private def getCDLExpressionAsStringWithType(e: CDLExpression, refType: ClaferReferenceType): String = {
    e match {
      case Identifier(s) => {
        if (referenceType.isInstanceOf[IntegerRef]) {
          "#" + getCDLExpressionAsString(e)
        } else {
          getCDLExpressionAsString(e)
        }
      }
      case Plus(first, second) => {
        getCDLExpressionAsStringWithType(first, referenceType) + " + " + getCDLExpressionAsStringWithType(second, referenceType)
      }

      case _ => {getCDLExpressionAsString(e)}
    }
  }

  private def guardString(s: String) : String = {
    s.trim.replaceAll("\"", "")
  }

  private def getCDLExpressionAsString (e : CDLExpression) : String = {
    getCDLExpressionAsString(e, false)
  }

  /**
   *
   * Converts CDLExpression to String
   * TODO: Refactor! Should be much smaller method
   *
   */
  private def getCDLExpressionAsString (e : CDLExpression, takeReferenceInAccount: Boolean) : String = {
    e match {
      case StringLiteral(value) => {
          if (takeReferenceInAccount && referenceType.isInstanceOf[EnumRef]) {
            referenceType.asInstanceOf[EnumRef].getEnumElementByKey(value) match {
              case Some(s: String) => {s}
              case None => {value}
            }
          } else {
            value
          }
      }
      case IntLiteral(value) => {
        if (takeReferenceInAccount && referenceType.isInstanceOf[EnumRef]) {
          referenceType.asInstanceOf[EnumRef].getEnumElementByKey(String.valueOf(value)) match {
            case Some(s: String) => {s}
            case None => {String.valueOf(value)}
          }
//        } else if (referenceType.isInstanceOf[IntegerRef]) {
        } else {
          String.valueOf(value)
        }
      }
      case Div(left, right) => {
        if (left.isInstanceOf[Identifier] && getCDLExpressionType(right).isInstanceOf[IntegerRef]) {
          "#" + getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " / " + getCDLExpressionAsString(right, takeReferenceInAccount)
        } else if (getCDLExpressionType(left).isInstanceOf[IntegerRef] && right.isInstanceOf[Identifier]) {
          getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " / #" + getCDLExpressionAsString(right, takeReferenceInAccount)
        } else {
          getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " / " + getCDLExpressionAsString(right, takeReferenceInAccount)
        }
      }
      case Dot(left, right) => {
        val builder = new StringBuilder
        builder.
          append("(").
          append(getCDLExpressionAsString(left, takeReferenceInAccount)).
          append(") ").
          append(concatenation).
//          append(newLineAndIndent(level + 2 + depth)).
          append("(").
          append(getCDLExpressionAsString(right, takeReferenceInAccount)).
          append(")").toString
      }
      case Eq(left: CDLExpression, right: CDLExpression) => {
        if (left.isInstanceOf[Identifier] && getCDLExpressionType(right).isInstanceOf[IntegerRef]) {
          "#" + getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " = " + getCDLExpressionAsString(right, takeReferenceInAccount)
        } else if (getCDLExpressionType(left).isInstanceOf[IntegerRef] && right.isInstanceOf[Identifier]) {
          getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " = #" + getCDLExpressionAsString(right, takeReferenceInAccount)
        } else if (left.isInstanceOf[StringLiteral] && right.isInstanceOf[Identifier]) {
          clafersMap.get(getCDLExpressionAsString(right)) match {
            case Some(claferReferenceHolder: ClaferReferenceHolder) =>  {
              if (claferReferenceHolder.referenceType.isInstanceOf[EnumRef]) {
                guardString(getCDLExpressionAsString(left, takeReferenceInAccount)) + 
                " = " + getCDLExpressionAsString(right, takeReferenceInAccount)
              } else {
                getCDLExpressionAsString(left, takeReferenceInAccount) +
                " = " + getCDLExpressionAsString(right, takeReferenceInAccount)
              }
            }
            case None => {
                  getCDLExpressionAsString(left, takeReferenceInAccount) + 
                  " = " + getCDLExpressionAsString(right, takeReferenceInAccount)
            }
          }
        } else if (left.isInstanceOf[Identifier] && right.isInstanceOf[StringLiteral]) { // ENUM?
          clafersMap.get(getCDLExpressionAsString(left)) match {  // there is
            case Some(claferReferenceHolder: ClaferReferenceHolder) =>  {
              if (claferReferenceHolder.referenceType.isInstanceOf[EnumRef]) {
                getCDLExpressionAsString(left, takeReferenceInAccount) + 
                " = " + guardString(getCDLExpressionAsString(right, takeReferenceInAccount))
              } else {
                getCDLExpressionAsString(left, takeReferenceInAccount) + 
                " = " + getCDLExpressionAsString(right, takeReferenceInAccount)
              }
            }
            case None => {
              getCDLExpressionAsString(left, takeReferenceInAccount) + 
              " = " + getCDLExpressionAsString(right, takeReferenceInAccount)
            }
          }
        } else {
          getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " = " + getCDLExpressionAsString(right, takeReferenceInAccount)
        }
      }
      /*
      * TODO: EQ and NEq are almost the same, optimize that and simplify. Also what happens if enum element is of Int type
      * TODO: This can throw an error
      * */
      case NEq(left, right) => {
        if (left.isInstanceOf[Identifier] && getCDLExpressionType(right).isInstanceOf[IntegerRef]) {
          "#" + getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " != " + getCDLExpressionAsString(right, takeReferenceInAccount)
        } else if (getCDLExpressionType(left).isInstanceOf[IntegerRef] && right.isInstanceOf[Identifier]) {
          getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " != #" + getCDLExpressionAsString(right, takeReferenceInAccount)
        } else if (left.isInstanceOf[StringLiteral] && right.isInstanceOf[Identifier]) {
          clafersMap.get(getCDLExpressionAsString(right)) match {
            case Some(claferReferenceHolder: ClaferReferenceHolder) =>  {
              if (claferReferenceHolder.referenceType.isInstanceOf[EnumRef]) {
                guardString(getCDLExpressionAsString(left, takeReferenceInAccount)) + 
                " != " + getCDLExpressionAsString(right, takeReferenceInAccount)
              } else {
                getCDLExpressionAsString(left, takeReferenceInAccount) +
                " != " + getCDLExpressionAsString(right, takeReferenceInAccount)
              }
            }
            case None => {
              getCDLExpressionAsString(left, takeReferenceInAccount) + 
              " != " + getCDLExpressionAsString(right, takeReferenceInAccount)
            }
          }
        } else if (left.isInstanceOf[Identifier] && right.isInstanceOf[StringLiteral]) { // ENUM?
          clafersMap.get(getCDLExpressionAsString(left)) match {  // there is
            case Some(claferReferenceHolder: ClaferReferenceHolder) =>  {
              if (claferReferenceHolder.referenceType.isInstanceOf[EnumRef]) {
                getCDLExpressionAsString(left, takeReferenceInAccount) + 
                " != " + guardString(getCDLExpressionAsString(right, takeReferenceInAccount))
              } else {
                getCDLExpressionAsString(left, takeReferenceInAccount) + 
                " != " + getCDLExpressionAsString(right, takeReferenceInAccount)
              }
            }
            case None => {
              getCDLExpressionAsString(left, takeReferenceInAccount) + 
              " != " + getCDLExpressionAsString(right, takeReferenceInAccount)
            }
          }
        } else {
          getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " != " + getCDLExpressionAsString(right, takeReferenceInAccount)
        }
      }
      case GreaterThanOrEq(left, right) => {
        if (left.isInstanceOf[Identifier] && getCDLExpressionType(right).isInstanceOf[IntegerRef]) {
          "#" + getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " >= " + getCDLExpressionAsString(right, takeReferenceInAccount)
        } else if (getCDLExpressionType(left).isInstanceOf[IntegerRef] && right.isInstanceOf[Identifier]) {
          getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " >= #" + getCDLExpressionAsString(right, takeReferenceInAccount)
        } else {
          getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " >= " + getCDLExpressionAsString(right, takeReferenceInAccount)
        }
      }
      case GreaterThan(left, right) => {
        if (left.isInstanceOf[Identifier] && getCDLExpressionType(right).isInstanceOf[IntegerRef]) {
          "#" + getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " > " + getCDLExpressionAsString(right, takeReferenceInAccount)
        } else if (getCDLExpressionType(left).isInstanceOf[IntegerRef] && right.isInstanceOf[Identifier]) {
          getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " > #" + getCDLExpressionAsString(right, takeReferenceInAccount)
        } else {
          getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " > " + getCDLExpressionAsString(right, takeReferenceInAccount)
        }
      }
      case LessThanOrEq(left, right) => {
        if (left.isInstanceOf[Identifier] && getCDLExpressionType(right).isInstanceOf[IntegerRef]) {
          "#" + getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " <= " + getCDLExpressionAsString(right, takeReferenceInAccount)
        } else if (getCDLExpressionType(left).isInstanceOf[IntegerRef] && right.isInstanceOf[Identifier]) {
          getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " <= #" + getCDLExpressionAsString(right, takeReferenceInAccount)
        } else {
          getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " <= " + getCDLExpressionAsString(right, takeReferenceInAccount)
        }
      }
      case LessThan(left, right) => {
        if (left.isInstanceOf[Identifier] && getCDLExpressionType(right).isInstanceOf[IntegerRef]) {
          "#" + getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " < " + getCDLExpressionAsString(right, takeReferenceInAccount)
        } else if (getCDLExpressionType(left).isInstanceOf[IntegerRef] && right.isInstanceOf[Identifier]) {
          getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " < #" + getCDLExpressionAsString(right, takeReferenceInAccount)
        } else {
          getCDLExpressionAsString(left, takeReferenceInAccount) + 
          " < " + getCDLExpressionAsString(right, takeReferenceInAccount)
        }
      }
      case Identifier(identifierString) => {
//        if (takeReferenceInAccount && referenceType.isInstanceOf[IntegerRef]) {
//          "#" + String.valueOf(identifierString)
//        } else {
          String.valueOf(identifierString)
//        }
      }
      case Not(s) => {"!" + s}
      case Plus(first, second) => {
        if (getCDLExpressionType(first).isInstanceOf[IntegerRef] || getCDLExpressionType(second).isInstanceOf[IntegerRef]) {
          if (first.isInstanceOf[Identifier]) {
            "#" + getCDLExpressionAsString(first, takeReferenceInAccount) + 
            " + " + getCDLExpressionAsString(second, takeReferenceInAccount)
          } else if (second.isInstanceOf[Identifier]) {
            getCDLExpressionAsString(first, takeReferenceInAccount) + 
            " + #" + getCDLExpressionAsString(second, takeReferenceInAccount)
          } else {
            getCDLExpressionAsString(first, takeReferenceInAccount) + 
            " + " + getCDLExpressionAsString(second, takeReferenceInAccount)
          }
        } else {
            "(" +
            getCDLExpressionAsString(first, takeReferenceInAccount) +
            " + " +
            getCDLExpressionAsString(second, takeReferenceInAccount) +
            ")"
        }
      }
      case Minus(first, second) => {
        if (getCDLExpressionType(first).isInstanceOf[IntegerRef] || getCDLExpressionType(second).isInstanceOf[IntegerRef]) {
          if (first.isInstanceOf[Identifier]) {
            "#" + getCDLExpressionAsString(first, takeReferenceInAccount) + 
            " - " + getCDLExpressionAsString(second, takeReferenceInAccount)
          } else if (second.isInstanceOf[Identifier]) {
            getCDLExpressionAsString(first, takeReferenceInAccount) + 
            " - #" + getCDLExpressionAsString(second, takeReferenceInAccount)
          } else {
            getCDLExpressionAsString(first, takeReferenceInAccount) + 
            " - " + getCDLExpressionAsString(second, takeReferenceInAccount)
          }
        } else {
            "(" +
            getCDLExpressionAsString(first, takeReferenceInAccount) +
            " - " +
            getCDLExpressionAsString(second, takeReferenceInAccount) +
            ")"
        }
      }
      case Or(left, right) => {
        getCDLExpressionAsString(left, takeReferenceInAccount) + 
        " || " + getCDLExpressionAsString(right, takeReferenceInAccount) 
      }
      case And(left, right) => {
        getCDLExpressionAsString(left, takeReferenceInAccount) + 
        " && " + getCDLExpressionAsString(right, takeReferenceInAccount) 
      }
      case Conditional(cond, pass, fail: CDLExpression) => {
        val builder = new StringBuilder
        builder.
          append("((").
          append(getCDLExpressionAsString(cond, takeReferenceInAccount)).
          append(")").
          append(" => ").
          append(getCDLExpressionAsString(pass, takeReferenceInAccount)).
          append(" else ").
          append(getCDLExpressionAsString(fail, takeReferenceInAccount)).
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

  private def appendDefaultValues(n: Node, builder: StringBuilder, depth: Int) = {
    n.defaultValue match {
      case Some(value) => {
        builder.
          append(newLineAndIndent(depth)).
          append("-- default_value = ").
          append(getCDLExpressionAsString(value, true))
      }
      case _ => {}
    }
  }

  private def getEnumNameForNode(n: Node): String = {
    "" + n.id + "_ENUM"
  }

  private def appendEnumDeclaration(builder: StringBuilder, n: Node): Unit = {
    if (referenceType.isInstanceOf[EnumRef]) {

      builder.
      append(newLine).
      append("enum ").
      append(referenceType.asInstanceOf[EnumRef].nodeName).
      append(" = ")
      var these = referenceType.asInstanceOf[EnumRef].getEnumElementsPrepared
      for {i <- 0 to referenceType.asInstanceOf[EnumRef].getEnumElementsPrepared.size - 1} {
        builder.append(these.head)
        these = these.tail
        if (i != referenceType.asInstanceOf[EnumRef].getEnumElementsPrepared.size - 1)
          builder.append(" | ")
      }
    }
  }

  /**
   *
   *
   */
  private def appendLegalValuesDeclaration(n: Node, builder: StringBuilder, depth: Int) = {
    n.legalValues match {
      case Some(legalValueOption: LegalValuesOption) => {
        legalValueOption.ranges.foreach(
          range_ => range_ match {
            case MinMaxRange(low, high) => {
              builder.
                append(newLineAndIndent(depth)).
                append("[")
                if (low.isInstanceOf[Identifier]) {
                  builder.append("#")
                }
                builder.append(getCDLExpressionAsString(low)).
                append(" <= this && this <= ")
                if (high.isInstanceOf[Identifier]) {
                  builder.append("#")
                }
                builder.append(getCDLExpressionAsString(high)).
                append("]")
            }

            case SingleValueRange(r) => {}
          }
        )
      }

      case None => {}
    }
  }

  private def getCDLExpressionType(expression : CDLExpression) : ClaferReferenceType = {
    expression match {
      case Conditional(cond, pass, fail) => {
          getCDLExpressionType(pass) //
      }
      case Dot(left, right) => {
          getCDLExpressionType(left) // both left and right should be fine
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
        if (!isOptionalByNature(n) || !isMandatoryByCalculated(n)) {
          builder.
            append(newLineAndIndent(depth)).
            append("-- calculated").
            append(newLineAndIndent(depth))
            if (!isCalculatedExpressionBoolean(expr)) {
              builder.append("[this = ")
            } else {
              builder.append("[")
            }
            builder.append(newLineAndIndent(depth)).
            append(getCalculatedExpressionAsString(expr, depth)).
            append("]")
        }
      }
      case None =>
    }
  }

  /*
  * Returns clafer type by parsing legal values
  * */
  private def getClaferReferenceTypeFromLegalValues(n: Node): ClaferReferenceType = {
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
   */
  private def getClaferReferenceType(n: Node): ClaferReferenceType = {
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
             */
            getCDLExpressionType(leafNodes.apply(0))
          } else {
            getCDLExpressionType(calculated)
          }
        }
        case None => {
          val referenceTypeFromLegalValues = getClaferReferenceTypeFromLegalValues(n)
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

  private def isAbstractNode(n: Node): Boolean = {
    n.cdlType == InterfaceType && n.flavor == DataFlavor
  }

  case class ClaferReferenceHolder (referenceType: ClaferReferenceType, name: String) {
    override def toString: String = {name}
  }

  var referenceType: ClaferReferenceType = new NoRef
  var abstractClafers = List[String]()
  var clafersMap = Map[String, ClaferReferenceHolder]()

  /**
   *  Main method that converts Node to clafer string
   */
  private def cdlNodeToClaferString(n : Node, depth : Int) : String = {
    var actualDepth =  0
    if (!isAbstractNode(n)) {
        actualDepth = depth
    }

    val builder = new StringBuilder
    builder.append(newLine)

    clafersMap.get(n.id) match {
      case Some (simpleClafer: ClaferReferenceHolder) => {referenceType = simpleClafer.referenceType}
      case None => {throw new Exception()}
    }

    appendFirstLineOfClafer(builder, actualDepth, n)
    appendDisplay(builder, actualDepth, n)
    appendImplements(n, actualDepth, builder)
    appendDescription(n, builder, actualDepth)
    appendDefaultValues(n, builder, actualDepth)
    appendLegalValuesDeclaration(n, builder, actualDepth)
    appendActiveIfs(n, actualDepth, builder)
    appendReqs(n, actualDepth, builder)
    appendCalculated(n, builder, actualDepth)
    appendEnumDeclaration(builder, n)

//    builder.append(newLineAndIndent(depth)).append("FLAVOR: ").append(n.flavor)
    if (isAbstractNode(n)) {
      abstractClafers += builder.toString
      builder.delete(0, builder.length) // empty the builder
    }

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
   */
  def asClaferString( topLevelNodes:List[Node] ) : String = {
    collectl {
      case n:Node => {
        clafersMap += (n.id -> new ClaferReferenceHolder(getClaferReferenceType(n), n.id))
      }
    }(topLevelNodes)

    val builder = new StringBuilder
    topLevelNodes.foreach(node => builder.append(cdlNodeToClaferString(node, 0)))

    val builderWithAbstract = new StringBuilder
    abstractClafers.foreach(node => builderWithAbstract.append(node))
    builderWithAbstract.append(builder.toString)

    builderWithAbstract.toString
  }

  /**
   * Reads IML file from inputFile
   * and returns ClaferReferenceHolder representation
   */
  def getClaferStringFromIMLFile( inputFile: String ) : String = {
    parseAll(cdl, new PagedSeqReader(PagedSeq fromFile inputFile)) match {
      case Success(res,_) => {
        asClaferString( res )
      }
      case x => ""
    }
  }
}

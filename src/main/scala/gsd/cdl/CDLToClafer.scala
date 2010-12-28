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
package gsd.cdl

import model._
import statistics.{Feature, Features, CDLModel}
import util.parsing.input.PagedSeqReader
import collection.immutable.PagedSeq
import kiama.rewriting.Rewriter
import scala.collection.mutable
import java.io._
import javax.xml.bind.{Unmarshaller, Marshaller, JAXBContext}
import scala.collection.jcl.Conversions._
import gsd.cdl.statistics._
import scala.Option

object CDLToClafer extends IMLParser with Rewriter {

  var nodesById = Map[String,Node]()
  var childParentMap = Map[String,String]()

  def main( args: Array[String] ){
    processInputFile("calculated.iml.txt", "")
  }

  def processInputFile (inputFile: String, outputFile: String) {
    parseAll(cdl, new PagedSeqReader(PagedSeq fromFile getBaseInputDir + inputFile)) match{
      case Success(res,_) => {
        val claferString = asClaferString( res )
        println(claferString)
        printToFile(claferString)
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
      append("-- ifActive")
      .append(" (").append(activeIf.getClass).append(")").     //TODO: Remove this line
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

  private def appendReqs (n: Node, depth: Int, builder : StringBuilder) = {
    n.reqs.foreach(req =>
        builder.append(newLine).append(indent(depth)).
        append("[").append(getCDLExpressionAsString(req)).append("]")
      )
  }

  private def appendFirstLineOfClafer(builder: StringBuilder, depth: Int, reference: Option[String], n: Node) = {
    builder.append(newLineAndIndent(depth - 1))
    if (n.cdlType == InterfaceType && n.flavor == DataFlavor) {
      builder.append("abstract ")
    }
    builder.append(n.id) // Clafer name
    if (n.implements.size == 1)
      builder.append(" extends ").append(getCDLExpressionAsString(n.implements.first))

    reference match {
      case Some(r) => {builder.append(" -> ").append(r).append(" ")}
      case None => {}
    }

    n.cdlType match {
      case OptionType => {
        if (n.flavor == BoolFlavor)
          builder.append(" ?")
      }
      case _ => {}
    }
  }

  private def getCDLExpressionAsString (e : CDLExpression) : String = {
    e match {
      case StringLiteral(value) => {
        if (value.substring(0, 1) == "\"" && value.substring(value.length - 1, value.length) == "\"") {
          value.substring(1, value.length - 1)
        }  else {
          value
        }
      }
      case IntLiteral(value) => {String.valueOf(value)}
      case Eq(left, right) => {
        if (left.isInstanceOf[Identifier]) {
          "#" + getCDLExpressionAsString(left) + " = " + getCDLExpressionAsString(right)
        } else if (right.isInstanceOf[Identifier]) {
          getCDLExpressionAsString(left) + " = #" + getCDLExpressionAsString(right)
        } else {
          getCDLExpressionAsString(left) + " = " + getCDLExpressionAsString(right)
        }
      }
      case Identifier(s) => {s}
      case Not(s) => {"!" + s}
      case Or(left, right) => {getCDLExpressionAsString(left) + " || " + getCDLExpressionAsString(right) }
      case Conditional(cond, pass, fail) => {"[" + getCDLExpressionAsString(cond) + " ? " + getCDLExpressionAsString(pass) + " : " + getCDLExpressionAsString(fail) + "]"}
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
    builder.append(newLineAndIndent(depth)).append("display = " + n.display.toString)
  }

  private def appendDefaultValues(n: Node, builder: StringBuilder, depth: Int) = {
    n.defaultValue match {
      case Some(value) => {
        builder.append(newLineAndIndent(depth)).append("-- default_value = ").append(getCDLExpressionAsString(value))
      }
      case _ => {}
    }
  }

  private def getEnumNameForNode(n: Node): String = {
    "" + n.id + "_ENUM"
  }

  private def appendEnumDeclaration(enum: List[String], builder: StringBuilder, n: Node): Unit = {
    if (!enum.isEmpty) {
      builder.append(newLine).append("enum ").append(getEnumNameForNode(n)).append(" = ")
      var these = enum
      for {i <- 0 to enum.size - 1} {
        builder.append(these.head)
        these = these.tail
        if (i != enum.size - 1)
          builder.append(" | ")
      }
    }
  }

  private def appendLegalValuesDeclaration(legalValues: List[String], builder: StringBuilder, depth: Int) = {
    if (!legalValues.isEmpty) {
      legalValues.foreach(builder.append(newLineAndIndent(depth)).append(_))
    }
  }

  private def getReference(expression : CDLExpression) : String = {
    expression match {
      case Conditional(cond, pass, fail) => {
          getReference(pass)
      }
      case IntLiteral(s) => {
        "integer"
      }
      case StringLiteral(value) => {
        if (value.substring(1, 2) == "x") {
          "integer"
        } else {
          "string"
        }
      }
      case Plus(left, right) => {"integer"} // check
      case Minus(left, right) => {"integer"} // check
      case _ => "unknown (" + expression.getClass.toString + ")"  // error! TODO: cover all cases
    }
  }

  private def getReference(enum: List[String], n: Node): Option[String] = {
      if (n.flavor != BoolFlavor) {
        if (!enum.isEmpty) {
          Some(getEnumNameForNode(n))
        } else {
          n.defaultValue match {
            case Some(expression) => {
              Some(getReference(expression))
            }
            case None => {
              None
            }
          }
        }
      } else {
        None
      }
  }

  private def appendCalculated(n: Node, builder: StringBuilder, depth: Int): Unit = {
    n.calculated match {
      case Some(expr) => {
        builder.append(newLineAndIndent(depth)).append("calculated: ").append(getCDLExpressionAsString(expr))
      }
      case None =>
    }
  }

  private def cdlNodeToClaferString(n : Node, depth : Int) : String = {
    val builder = new StringBuilder
    builder.append(newLine)

    var enum = List[String]()
    var legalValues = List[String]()

    n.legalValues match {
      case Some(legalValueOption: LegalValuesOption) => {
          legalValueOption.ranges.foreach(
             range_ => range_ match {
               case SingleValueRange(value) => {
                 enum += getCDLExpressionAsString(value)
               }
               case MinMaxRange(low, high) => {
                 val b = new StringBuilder
                 legalValues += b.append("[").append(low).append(" <= this && this <= ").append(high).append("]").toString
               }
             }
          )
        }

      case None => {}
    }

    appendFirstLineOfClafer(builder, depth, getReference(enum, n), n)
    appendDisplay(builder, depth, n)
    appendImplements(n, depth, builder)
    appendDescription(n, builder, depth)
    appendDefaultValues(n, builder, depth)
    appendLegalValuesDeclaration(legalValues, builder, depth)
    appendActiveIfs(n, depth, builder)
    appendReqs(n, depth, builder)
    appendCalculated(n, builder, depth)
    appendEnumDeclaration(enum, builder, n)

//    builder.append(newLineAndIndent(depth)).append("FLAVOR: ").append(n.flavor)

    //recursively print children
    n.children.foreach(child => builder.append(cdlNodeToClaferString(child, depth + 1)))

    builder.toString
  }

  private def printToFile(text: String): Unit = {
    val file = getBaseOutputDir + "clafer.txt"
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

  def asClaferString( topLevelNodes:List[Node] ) : String = {
    val builder = new StringBuilder
    topLevelNodes.foreach(node => builder.append(cdlNodeToClaferString(node, 0)))

    builder.toString
  }

}

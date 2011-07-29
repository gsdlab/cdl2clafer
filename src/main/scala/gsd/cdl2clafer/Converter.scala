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
import gsd.cdl.formula._
import gsd.cdl2clafer.model._
import gsd.cdl2clafer.utils._
import org.kiama.rewriting.Rewriter._
import gsd.cdl.formula.types.checker._
import gsd.cdl.formula.types.Type

import scala.collection._

object Converter {
  
  def createNonBoolInterfaceNode(id:String):Node = {
    Node(ArtificialInterfaceVariableName(id), 
        OptionType,
        "",
        None,
        DataFlavor,
        None,
        Some(LongIntLiteral(32)),
        None,
        List(),
        List(),
        List(),
        List())
  }

  def createBoolInterfaceNode(id:String):Node = {
    Node(ArtificialInterfaceVariableName(id), 
        OptionType,
        "",
        None,
        BoolFlavor,
        None,
        None,
        None,
        List(),
        List(),
        List(),
        List())
  }
  
  
  /**
   * Convert IML model from file @file to list of nodes 
   **/
 def convert(file:String):List[ClaferNode] = {
 
    val nodes = gsd.cdl.parser.EcosIML.parseFile(file)
 
    val allNodes:List[Node] = collectl { case n:Node => n }(nodes).map(_ match {
        case 	Node(id, PackageType, display, description, flavor,     defaultValues, calculated, legalValues, reqs, activeIfs, implements, children) => 
        		Node(id, PackageType, display, description, NoneFlavor, defaultValues, calculated, legalValues, reqs, activeIfs, implements, children)
        case x@_ => x
      } )
      
    var allNodesNew = mutable.ListBuffer[Node]()
    
    allNodesNew ++= allNodes
    // we create new iml nodes thet replace Interface variables
    // this way we 'trick' the type inference
    // that wouldn't consider interfaces otherwise
    allNodes.foreach(node => {
      if (node.cdlType == InterfaceType) {
        if (node.flavor != BoolFlavor) {
    	  allNodesNew +=  createNonBoolInterfaceNode(node.id)
        } else {
          allNodesNew +=  createBoolInterfaceNode(node.id)
        }
      }
    })
    
    val allNodesMap:immutable.Map[String, Node] = allNodes.map(n => (n.id, n)).toMap[String, Node]
    
    val (constraints, usedConstraintsIndexes) = getNodesConstraints(allNodes, allNodesMap)
    
    val typeChecker = new TypeChecker(allNodesNew.toList, constraints.toList)
    val typeErrors = typeChecker.verify() // do the checking
    var inferencedConstraints = typeChecker.getTypedForest
    
    inferencedConstraints = inferencedConstraints.dropRight(allNodes.size)
    
    val newSymbolTable = getFullSymbolTable(allNodesMap, typeChecker.getSymbolTable)
    val constraintsMap = generateConstraintMap(usedConstraintsIndexes, inferencedConstraints)
    
    // add clafer interfaces
    var claferNodes:mutable.ListBuffer[ClaferNode] = mutable.ListBuffer[ClaferNode]()
    claferNodes = claferNodes ++ allNodes.filter(n => n.cdlType == InterfaceType).map(n => {
      createAbstractNode(n)
    })
    
    claferNodes = claferNodes ++ nodes.map(node => {IMLNode2ClaferNode(node, allNodesMap, newSymbolTable, constraintsMap)})
    claferNodes.toList
//    nodes.map(node => {IMLNode2ClaferNode(node, allNodesMap, newSymbolTable, constraintsMap)})
 }
 
 def createAbstractNode(n:Node):ClaferNode = {
  ClaferNode(AbstractVariableName(n.id), true,  true, types.BoolType, "Interface for node: " + n.id, None,
		  getAbstractNodeConstraint(n), // no constraints
		  None, List(), List()
  )
 }
 
 private def getAbstractNodeConstraint(n:Node):List[GExpression] = {
   if (n.flavor != DataFlavor) {
	  List(GImplies(
	          GEq(GNumImplementations(GVariable(AbstractVariableName(n.id))), GLongIntLiteral(0)),
	          GClaferNoInstances(GVariable(n.id))
	          )
	  ) // constraints
   } else {
     List()
   }
 } 
 
  def countCastings(constraint:GExpression 
//     allNodesMap:scala.collection.immutable.Map[String, Node]
  ):Int = {
	count { case gVar:gsd.cdl.formula.types.GCast => 1}(constraint)
 }
 
 
 private def getNodesConstraints(
     allNodes:List[Node],
     allNodesMap:immutable.Map[String, Node]) = {
    var constraints = mutable.ListBuffer[GExpression]()
    
    var usedConstraintsIndexes = 
      mutable.Map[String, mutable.ListBuffer[Int]]()
    
    allNodes.foreach(node => {
       usedConstraintsIndexes += (node.id -> mutable.ListBuffer[Int]())
       
       node.reqs.foreach(c => {
          val constraint = CDLToGExpression(c, allNodesMap)
       	  constraints += Rewriters.addInterfaceSuffix(constraint, allNodesMap) 
       	  usedConstraintsIndexes.apply(node.id) += (constraints.size - 1)
       })

       node.activeIfs.foreach(c => {
		val constraint = CDLToGExpression(c, allNodesMap)
		constraints += Rewriters.addInterfaceSuffix(constraint, allNodesMap) 
		usedConstraintsIndexes.apply(node.id) += (constraints.size - 1)
       })   
       
       if (node.calculated != None) {
        if (node.flavor == DataFlavor) {
			val constraint = GVariable(DataVariableName(node.id)) === CDLToGExpression(node.calculated.getOrElse(null), allNodesMap)
			constraints += Rewriters.addInterfaceSuffix(constraint, allNodesMap) 
			usedConstraintsIndexes.apply(node.id) += (constraints.size - 1)
       	}
       }

       if (node.legalValues != None) {
			val constraint = LegalValuesToGExpression(node, allNodesMap)
			constraints += Rewriters.addInterfaceSuffix(constraint.getOrElse(null), allNodesMap) 
			usedConstraintsIndexes.apply(node.id) += (constraints.size - 1)
       }
    })
    
    // WARNING: HACK!
    // This adds a simple constraint for each node
    // because type inference doesn't consider the nodes
    // not occurring in any constraint. 
    allNodes.foreach(node => {
      if (node.flavor == DataFlavor || node.flavor == BoolDataFlavor)
        constraints += GVariable(DataVariableName(node.id))
      else 
        constraints += GVariable(BoolVariableName(node.id))
    })    
    
    (constraints, usedConstraintsIndexes)
 }
 
 /**
  * This method returns types inferred by the type inference engine.
  * 
  * It replaces custom variable names with original node id's.
  **/
 private def getFullSymbolTable(
     allNodesMap:immutable.Map[String, Node],
     symbolTable:SymbolTable) = {
     
        val newSymbolTable = mutable.Map[String, mutable.Set[Type]]()
    
		symbolTable.getMap.foreach(pair => {
		  val removeBoolSuffx = 
		    BoolVariableName.unapply(pair._1).getOrElse(pair._1)
		  val withoutDataAndBoolSuffixes = DataVariableName.unapply(removeBoolSuffx).getOrElse(removeBoolSuffx)
		  
		  if (ArtificialInterfaceVariableName.unapply(withoutDataAndBoolSuffixes) == None) {
		    newSymbolTable.put(withoutDataAndBoolSuffixes, pair._2)
		  }
		})
		
		newSymbolTable
 }
 
 private def generateConstraintMap(
     usedConstraintsIndexes:mutable.Map[String, mutable.ListBuffer[Int]],
     convertedExpressions: List[GExpression]
 ):mutable.Map[String, mutable.ListBuffer[GExpression]] = {
    var constraintsMap:mutable.Map[String, mutable.ListBuffer[GExpression]]
       = mutable.Map[String, mutable.ListBuffer[GExpression]]() 
    
       
    // do the rewritings
    var newConstraints = convertedExpressions.map(constraint => {
      Rewriters.replaceGCasts(
         Rewriters.removeInterfaceSuffix(
             Rewriters.rewriteGVariable(constraint)
         )
      )
    })
    
    // do the casting rewritings
    
//    inferencedConstraints = inferencedConstraints.map(Rewriters.replaceGCasts(_))
    
    // do the values guardings 
//    newConstraints.foreach(c => {
//      if (countCastings(c) != 0) {
//        println(ClaferPrettyPrinter.pretty(c))
//      }
//    })
    // do the conditionals rewritings    
    
    usedConstraintsIndexes.foreach(nodeIndexPair => {
      if (!constraintsMap.contains(nodeIndexPair._1)) {
        constraintsMap += 
          (nodeIndexPair._1 -> mutable.ListBuffer[GExpression]())
      }
      
      nodeIndexPair._2.foreach(index => {
        constraintsMap.apply(nodeIndexPair._1) += 
          newConstraints.apply(index) 
      })
    
    })
    
    constraintsMap
 }
 
 /**
  * Prints IML model from file @file
  * as Clafer model 
  **/ 
 def printIMLAsClafer(file:String) = {
   convert(file).foreach(node =>
	   println(ClaferPrettyPrinter.pretty(node))
   )
 }
}

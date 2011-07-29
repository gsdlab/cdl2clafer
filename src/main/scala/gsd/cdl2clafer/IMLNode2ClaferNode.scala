
package gsd.cdl2clafer

import gsd.cdl.model._

import gsd.cdl2clafer.model._
import gsd.cdl2clafer.utils._
import gsd.cdl.formula._

import scala.collection._

import gsd.cdl.formula.types._

/**
 * @author: mnovakovic
 *
 *
 *
 * WARNING: All CDL Nodes are expected to be unique across the model!
 * 
 */



/**
 * Does final conversion to ClaferNode
 * 
 * Note: All constraints should be polished before this constructor is called
 * This just puts the things together, doesn't do any smart job
 **/
object IMLNode2ClaferNode {
  
 def apply(
     n:Node, 
     allNodesMap:immutable.Map[String, Node],
     symbolTable: mutable.Map[String, mutable.Set[gsd.cdl.formula.types.Type]],
     constraintsMap:mutable.Map[String, mutable.ListBuffer[GExpression]]) : ClaferNode = {
   
   
   if (isAbstract(n)) {
	  return ClaferNode(
	        n.id, 
	        (n.flavor == DataFlavor),
	        false, // not abstract
	        getInterfaceType(n), 
	        n.display, 
	        n.description,
	        getInterfaceConstraints(n, constraintsMap),
	        None,
	        List(),
	        getChildren(n, allNodesMap, symbolTable, constraintsMap)
	  )
   } else {
	  return ClaferNode(
	        n.id, 
	        isMandatory(n),
	        isAbstract(n),
	        getClaferType(n, symbolTable), 
	        n.display, 
	        n.description,
	        getConstraints(n, allNodesMap, symbolTable, constraintsMap),
	        None,
	        getImplements(n),
	        getChildren(n, allNodesMap, symbolTable, constraintsMap)
	  )
   }
 }
 
 private def getImplements(n:Node):List[GExpression] = {
   n.implements.foreach(implements => {
     
   })
   List()
 }
 
 private def getInterfaceConstraints(n:Node,
     constraintsMap:mutable.Map[String, mutable.ListBuffer[GExpression]]):List[GExpression] = {
   val constraints = mutable.ListBuffer[GExpression]()
   if (n.flavor != BoolFlavor) {
   	constraints += GEq(GVariable(n.id), GNumImplementations(GVariable(AbstractVariableName(n.id))))
   }
   
   if (n.flavor == BoolFlavor || n.flavor == BoolDataFlavor) {
     constraints += GGreaterThan(GNumImplementations(GVariable(AbstractVariableName(n.id))), GLongIntLiteral(0))
   }
   
   return (constraints ++ constraintsMap.apply(n.id).toList).toList
 }
 
 private def getInterfaceType(n:Node):types.Type = {
	 if (n.flavor == BoolFlavor) { 
	   types.BoolType   
	 } else {
	   types.NumberType
	 }
 }
 
 private def getClaferType(node:Node, 
     symbolTable: mutable.Map[String, mutable.Set[Type]]):Type = {
   if (symbolTable.contains(node.id)) {
     if (symbolTable.apply(node.id).size > 1) {
       DisjunctiveType
     } else {
       symbolTable.apply(node.id).head match {
         case gsd.cdl.formula.types.StringType => StringType
         case gsd.cdl.formula.types.BoolType => BoolType
         case gsd.cdl.formula.types.NumberType => NumberType
       }
     }
   } else {
     UndefinedType
   }
 }
 
 private def getConstraints(
     n:Node, 
     allNodesMap:immutable.Map[String, Node],
     symbolTable: mutable.Map[String, mutable.Set[gsd.cdl.formula.types.Type]],
     constraintsMap:mutable.Map[String, mutable.ListBuffer[GExpression]] 
 ):List[GExpression] = {
   constraintsMap.apply(n.id).toList
 }

 
 /**
  * Appends and convert children of this Clafer node
  **/ 
 private def getChildren(
     n:Node, 
     allNodesMap:immutable.Map[String, Node],
     symbolTable: scala.collection.mutable.Map[String, scala.collection.mutable.Set[gsd.cdl.formula.types.Type]],
     constraintsMap:scala.collection.mutable.Map[String, scala.collection.mutable.ListBuffer[GExpression]] 
 ):List[ClaferNode] = {
  var buffer = scala.collection.mutable.ListBuffer[ClaferNode]()
  n.children.foreach(buffer += IMLNode2ClaferNode(_, allNodesMap, symbolTable, constraintsMap))
  buffer.toList
 }
 
 /**
  * Interfaces are abstract 
  **/
 private def isAbstract(n:Node):Boolean = n.cdlType == InterfaceType
 
  /**
   * A node is mandatory if it is of Data or None Flavor, 
   * and if it doesn't have requires and active if constraints.
   */
  private def isMandatory(n:Node): Boolean = {
   if (isAbstract(n)) {
	   true
   } else { 
	   ((n.flavor == DataFlavor) || (n.flavor == NoneFlavor)) && //selected
	   n.activeIfs.size == 0 && //active
	   n.reqs.size == 0 // should we allow this to happen?
     }
  } 
}

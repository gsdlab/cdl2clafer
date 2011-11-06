/*
import gsd.cdl.model._
import gsd.cdl.formula._
import gsd.cdl2clafer.model._
import gsd.cdl2clafer.utils._
import org.kiama.rewriting.Rewriter._
import gsd.cdl.formula.types.checker._
import gsd.cdl.formula.types.Type

import scala.collection._

object Converter {


object Thorsten {

   def main( args: Array[String] ){

    val file = "input/iml/pc_vmWare.iml";

    val nodes = gsd.cdl2clafer.parser.EcosIML.parseFile(file)
 
    val allNodes:List[Node] = collectl { case n:Node => n }(nodes).map(_ match {
        case 	Node(id, PackageType, display, description, flavor,     defaultValues, calculated, legalValues, reqs, activeIfs, implements, children) => 
        		Node(id, PackageType, display, description, NoneFlavor, defaultValues, calculated, legalValues, reqs, activeIfs, implements, children)
        case x@_ => x
      } )
      
    val allNodesMap:immutable.Map[String, Node] = allNodes.map(n => (n.id, n)).toMap[String, Node]
    
    val (constraints, usedConstraintsIndexes) = getNodesConstraints(allNodes, allNodesMap)
    
    val typeChecker = new TypeChecker(allNodesNew.toList, constraints.toList)
    val typeErrors = typeChecker.verify() // do the checking
    var inferencedConstraints = typeChecker.getTypedForest
    
    inferencedConstraints = inferencedConstraints.dropRight(allNodes.size)
    
    val newSymbolTable = getFullSymbolTable(allNodesMap, typeChecker.getSymbolTable)
    val constraintsMap = generateConstraintMap(usedConstraintsIndexes, 
        inferencedConstraints, 
        allNodesMap, 
        newSymbolTable)
    
    // CDL interfaces as clafer abstract features
    var claferNodes:mutable.ListBuffer[ClaferNode] = mutable.ListBuffer[ClaferNode]()
    claferNodes = claferNodes ++ allNodes.filter(n => n.cdlType == InterfaceType).map(n => {
      createAbstractNode(n)
    })
   }

 private def getNodesConstraints(
     allNodes:List[Node],
     allNodesMap:immutable.Map[String, Node]) = {
    var constraints = mutable.ListBuffer[GExpression]()
    
    allNodes.foreach(node => {
       node.reqs.foreach(c => {
       	  constraints += c
       })

       node.activeIfs.foreach(c => {
      		constraints += c
       })   
       
       
       if (node.calculated != None) {
              constraints += node.calculated
       }

       if (node.defaultValue != None) {
//              constraints += node.defaultValue
       }

    })
    
    (constraints, usedConstraintsIndexes)
 }


}

*/

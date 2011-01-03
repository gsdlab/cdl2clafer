package gsd.cdl

/**
 * Created by IntelliJ IDEA.
 * User: marko
 * Date: 02/01/11
 * Time: 10:35 PM
 * To change this template use File | Settings | File Templates.
 */

object CDL2ClaferRun {
  def main( args: Array[String] ){
//    processCDLFile("problems.iml", "clafer.txt")
//    val cdl2clafer = new CDLToClafer
    gsd.cdl.CDLToClafer.processCDLFile("problems.iml", "clafer.txt")
  }
}
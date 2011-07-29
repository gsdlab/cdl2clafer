package gsd.cdltest

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

//@RunWith(classOf[JUnitRunner])
class CDL2ClaferTest extends FunSuite {

  /**
   * File to file testing.
   * Specify directory with in/out files
   * Specify pair of input/output files we want to test
   * Test file by file: take input, generate clafer out of it and then compare it
   * line by line with supposed output
   */

  val inputOutputFilesMap = mutable.Map[java.lang.String, java.lang.String](
  )

  private def getBaseInputDir = {
      System.getProperty("user.dir") + "/src/test/resources/file2file/"
  }
/*
  test("Test File To File transformation") {
    inputOutputFilesMap += ("test1.iml" -> "test1.cfr")

    inputOutputFilesMap.keys.foreach(
        (inputFile) => {
          // load iml from file, convert it to clafer and return list of clafer strings
          var resultList = List[String]()
          List.fromArray(gsd.cdl2clafer.CDL2Clafer.getClaferStringFromIMLFile(getBaseInputDir + inputFile).split("\n")).
          foreach(
            el => {
                        if (el.trim() != "") {
                          resultList += el.trim
                        }
            }
          )

          var supposedResultList = List[String]()
          scala.io.Source.fromFile(getBaseInputDir + inputOutputFilesMap(inputFile)).getLines.toList.
            foreach(
            el => {
                        if (el.trim() != "") {
                          supposedResultList += el.trim
                        }
            }
          )
//        resultList.foreach(println)
//        supposedResultList.foreach(println)

          // sizes should be the same
          assert(resultList.size == supposedResultList.size)

          for (i <- 0 to resultList.size - 1) {
            println("Comparing: [" + resultList.apply(i) + "] to [" + supposedResultList.apply(i) + "]")
            assert(resultList.apply(i) == supposedResultList.apply(i))
          }
        }
    )
  }
  */
}

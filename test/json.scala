/**************************************************************************************************
Rapture I/O Library
Version 0.8.0

The primary distribution site is

  http://www.propensive.com/

Copyright 2010-2013 Propensive Ltd.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing permissions and limitations under the
License.
  ***************************************************************************************************/

package test.rapture

import org.scalatest._
import org.scalatest.matchers.MustMatchers

import rapture.io._

import strategy.throwExceptions
import java.awt.Graphics2D

//import strategy.captureExceptions

class JsonSpec extends FlatSpec with MustMatchers {

  "Json" must "parse a json strings" in {
    val src = """{
        "candidates": [
        {
          "name": "Mitt Romney",
          "age": 65,
          "party": "Republican"
        },
        {
          "name": "Barack Obama",
          "age": 51,
          "party": "Democrat"
        }
        ]
      }"""
   var json = Json.parse(src)

    val n: Int = json.candidates.length-1
    for (i <- 0 to n) {
      println(json.candidates(i).name + ":" + json.candidates(i).age.toString + ":" + json.candidates(i).party)
    }

    def printType(z:Any) = {println(z.asInstanceOf[AnyRef].getClass.getCanonicalName)}

    def printJson(c:Any) = c match {
        case j: Json => println( j.name + ":" + j.age.toString + ":" + j.party)
        case s: String => println(s)
        //case a:Tuple2[Any, Any] =>
        case a:(Any, ::[Any]) =>
          printType(a._1)
          printType(a._2)
          println(a._1)
         // a._2.toMap.iterator.foreach((k,v) => println(k+":"+v))
          a._2 match {
            case b: ::[Any] => // warning at compilation
            //case b: $colon$colon[Any] =>
             /*      b.iterator.foreach{
                     str => str match {
                       case s: String => println(s)
                       case scala.collection.immutable.Map.Map3
                       case z:Any => printType(z)
                     }
                   }  */
            case z:Any => printType(z)
          }    */
        case z:Any => printType(z)
    }
    json.candidates.iterator.foreach((c) => printJson(c))

    val party = json.candidates(1).party.getString
    party must equal ("Democrat")

    //  val age = json.candidates(0).age.toDouble
    //  age must equal (65.0)


      json = Json.parse("""{"name": "2"}""")
      json.name.toInt must equal (2) // 2.0

      json = Json.parse("""{"abc": "aaa"}""")
      json.abc.toString must not equal ("aba") // must fail  */

      //json = Json.parse("{'abc': 'aaa'") must produce [NoSuchElementException]

    var str = """ { "foo": "bar" } """
    //Json.parse(json).foo.get[Option[String]]

  }

  "Encodings" must "be UTF-8" in {
    implicit val enc = Encodings.`UTF-8`
  }
}

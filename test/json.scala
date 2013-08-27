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

package rapture
import collection.mutable.Stack
import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import rapture._

class JsonSpec extends FlatSpec with ShouldMatchers {

  "Json" should "parse a json an int" in {
    var json = Json.parse("{'name': 2}")
    json.name.toInt should equal (2)
    json = Json.parse("{'name': 'aaa'}")
    json.name.toString should equal ("aaa")
  }

  /*it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[String]
    evaluating { emptyStack.pop() } should produce [NoSuchElementException]
  } */
}

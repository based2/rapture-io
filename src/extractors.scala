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

package rapture.implementation

import rapture._

import scala.xml._;

/** Defines some extractors for retrieving data from character streams */
trait Extracting extends Slurping {

  /** Defines extractors for getting Scala XML from a String or Input[Char]. The stream extractor
    * simply collects the input into a String, then parses the String, so more efficient
    * implementation may be possible. .*/
  object Xml {
    
    def unapply(in: Input[Char]): Option[Seq[Node]] = try {
      val so = new StringOutput
      in > so
      unapply(so.buffer)
    } catch { case e: Exception => None }

    def unapply(in: String): Option[Seq[Node]] =
      try { Some(XML.loadString(in)) } catch { case e: Exception => None }
  }
}


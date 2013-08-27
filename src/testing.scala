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
import scala.collection.mutable.ListBuffer

trait Testing extends CommandLineExtras {

  trait TestApp {
    private val suites = new ListBuffer[Suite]
    
    def run() = {
      val total = suites.foldLeft(0 -> 0) { case (t, s) =>
        val r = s.run()
        (t._1 + r._1) -> (t._2 + r._2)
      }
      hr("=")
      testOutput((if(color) Ansi.Bold else "")+"  TOTAL: "+total._1+"/"+total._2+" tests passed ("+
          (100*total._1/total._2)+"%)"+(if(color) Ansi.Normal else ""))
      hr("=")
    }

    def testOutput(s: String): Unit = println(s)
    protected def hr(c: String) = testOutput((if(color) Ansi.Black+Ansi.Bold else "")+(c*80)+
        (if(color) Ansi.Normal else ""))
    
    def color: Boolean = true

    class Suite(val name: String) {

      suites += this

      private var setUp: Option[() => Unit] = None
      private val tests: ListBuffer[Test[_]] = ListBuffer()
      private var tearDown: Option[() => Unit] = None
      
      def run(): (Int, Int) = {
        
        hr("-")
        testOutput("  "+(if(color) Ansi.Bold else "")+name+(if(color) Ansi.Normal else ""))
        hr(" ")
        var successes = 0
        setUp.foreach(_())
        tests foreach { t =>
          val tr = TestResult(t.name, try Some(t.doCheck()) catch { case e: Throwable => None })
          if(tr.success.getOrElse(false)) successes += 1
          testOutput(tr.toString)
        }
        tearDown.foreach(_())
        hr(" ")
        testOutput("  "+successes+"/"+tests.length+" tests passed ("+(100*successes/List(1,
            tests.length).max)+"%)")
        testOutput("")
        (successes, tests.length)
      }
    
      def test[T](blk: => T) = new TestDef[T](blk)
      class TestDef[T](blk: => T) {
        def throws[T <: Throwable](implicit mf: Manifest[T]): Test[Boolean] =
          define(new Test[Boolean] {
          
          def run(): Boolean = try { blk; false } catch {
            case e: T => true
            case e: Throwable => false
          }
          
          def check(t: Boolean): Boolean = t
        })
       
        def yields(y: => T): Test[T] = define(new Test[T] {
          def run(): T = blk
          def check(t: T): Boolean = t == y
        })

        def satisfies(sat: T => Boolean): Test[T] = define(new Test[T] {
          def run(): T = blk
          def check(t: T): Boolean = sat(t)
        })
        
        private def define[T](test: Test[T]) = {
          tests += test
          test
        }
      }

      abstract class Test[T] {
        def name: String = fields.get(this).getOrElse("unknown")
        def run(): T
        def check(t: T): Boolean
        private[Suite] def doCheck(): Boolean = check(run())
      }

      case class TestResult(name: String, success: Option[Boolean]) {
        override def toString() = {
          val result = success match {
            case Some(true) =>
              if(color) "[ "+Ansi.Green+"SUCCESS"+Ansi.Normal+" ]" else "[ SUCCESS ]"
            case Some(false) =>
              if(color) "[ "+Ansi.Red+"FAILURE"+Ansi.Normal+" ]" else "[ FAILURE ]"
            case None =>
              if(color) "[  "+Ansi.Yellow+"ERROR"+Ansi.Normal+"  ]" else "[  ERROR  ]"
          }
          val desc = if(name.length > 63) name.substring(0, 63) else name+(" "*(63 - name.length))
          "    "+desc+result
        }
      }

      lazy val fields: Map[Test[_], String] = this.getClass.getMethods.filter({ m =>
        m.getReturnType == classOf[Test[_]] && m.getParameterTypes.length == 0
      }).map({ m =>
        m.invoke(this).asInstanceOf[Test[_]] -> m.getName
      }).toMap
    }
    
    def main(args: Array[String]): Unit = run()
  }

}


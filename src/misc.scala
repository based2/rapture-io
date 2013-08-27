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

import scala.collection.mutable.HashMap
import scala.collection.generic._

import language.higherKinds

trait Misc {

  /** Provides a simple class mixin for creating a list of items from which items can be looked up.
    *
    * @tparam Index The type of the key by which items are indexed */
  trait Lookup[Index] {

    type Item <: AutoAppend

    trait AutoAppend { thisItem: Item =>
      def index: Index
      items(index) = thisItem
    }

    private val items = new HashMap[Index, Item]
    def elements = items.valuesIterator
    def lookup(idx: Index): Option[Item] = items.get(idx)
  }

  object Cell {
    def apply[T](get: => T)(set: T => Unit): Cell[T] = new Cell[T] {
      def apply() = get
      def update(t: T) = set(t)
    }
  }

  object Var {
    def apply[T](t: T) = new Cell[T] {
      private var value = t
      def apply(): T = value
      def update(t: T) = value = t
    }
  }

  trait Cell[T] {
    def apply(): T
    def update(t: T): Unit
  }

  class Counter {
    private var n = 0
    def apply() = synchronized { n += 1; n }
  }

  def repeat[T](blk: => T) = new { def until(test: T => Boolean) = {
    var t: T = blk
    while(!test(t)) t = blk
    t
  } }

  object load {
    def apply[C](implicit mf: scala.reflect.ClassTag[C]) = { mf.toString; () }
  }

  case class Csv(data: Array[Array[String]]) {
    override def toString = {
      val sb = new StringBuilder
      for(xs <- data) sb.append(xs.map(_.replaceAll("\"", "\\\"")).mkString("\"", "\",\"", "\"\n"))
      sb.toString
    }

    def rows = data.length
    def cols = data.headOption.map(_.length).getOrElse(0)

  }

  def yCombinator[A, B](fn: (A => B) => (A => B)): A => B = fn(yCombinator(fn))(_)

  /** Times how long it takes to perform an operation, returning a pair of the result and the
    * duration of the operation in milliseconds. */
  def time[T](blk: => T): (T, Long) = {
    val t = System.currentTimeMillis
    blk -> (System.currentTimeMillis - t)
  }
  
  @inline implicit class NullableExtras[T](t: T) {
    def fromNull = if(t == null) None else Some(t)
  }

  @inline implicit class SeqExtras[A, C[A] <: Seq[A]](val xs: C[A]) {

    /** Inserts an element between each of the elements of the sequence. */
    def intersperse[B >: A, That](between: B)(implicit bf: CanBuildFrom[C[A], B, That]): That = {
      val b = bf(xs)
      xs.init foreach { x =>
        b += x
        b += between
      }
      b += xs.last
      b.result
    }

    /** Inserts an element between each of the elements of the sequence, and additionally prepends
      * and affixes the sequence with `before` and `after`. */
    def intersperse[B >: A, That](before: B, between: B, after: B)
        (implicit bf: CanBuildFrom[C[A], B, That]): That = {
      val b = bf(xs)
      b += before
      xs.init foreach { x =>
        b += x
        b += between
      }
      b += xs.last
      b += after
      b.result
    }

    /** Convenience method for zipping a sequence with a value derived from each element. */
    def zipWith[T](fn: A => T)(implicit bf: CanBuildFrom[C[A], (A, T), C[(A, T)]]): C[(A, T)] = {
      val b = bf(xs)
      xs.foreach { x => b += (x -> fn(x)) }
      b.result
    }
  }


}

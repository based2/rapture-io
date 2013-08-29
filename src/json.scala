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

import language.dynamics
import scala.collection.mutable.{ListBuffer, HashMap}

/** Some useful JSON shortcuts */
trait JsonProcessing extends ExceptionHandling with Linking with Misc {

  case class ParseException(source: String, line: Option[Int] = None, column: Option[Int] = None)
      extends Exception {
    override def toString = "Failed to parse source"
  }

  /** Represents a JSON parser implementation which is used throughout this library */
  trait JsonParser {
    def parse(s: String): Option[Any]
    def parseMutable(s: String): Option[Any] = try Some(yCombinator[Any, Any] { fn =>
      _ match {
        case m: Map[_, _] =>
          val hm = HashMap[String, Any](m.asInstanceOf[Map[String, Any]].to[List]: _*)
          for(k <- hm.keys) hm(k) = fn(hm(k))
          hm
        case lst: List[_] => ListBuffer(lst.map(fn): _*)
        case x => x
      }
    } (parse(s).get)) catch { case e: Exception => None }
  }

  /** The default JSON parser implementation */
  implicit val ScalaJsonParser = new JsonParser {

    import scala.util.parsing.json._

    def parse(s: String): Option[Any] = JSON.parseFull(s)
  }

  /** Provides support for JSON literals, in the form json" { } " or json""" { } """. Interpolation
    * is used to substitute variable names into the JSON, and to extract values from a JSON string.
    */
  @inline implicit class JsonStrings(sc: StringContext)(implicit jp: JsonParser) extends {
    object json {
      /** Creates a new interpolated JSON object. */
      def apply(exprs: Any*)(implicit eh: ExceptionHandler): eh.![ParseException, Json] =
        eh.except {
          val sb = new StringBuilder
          val textParts = sc.parts.iterator
          val expressions = exprs.iterator
          sb.append(textParts.next())
          while(textParts.hasNext) {
            sb.append(new Json(expressions.next).toString)
            sb.append(textParts.next)
          }
          Json.parse(sb.toString)(jp, strategy.throwExceptions)
        }

      /** Extracts values in the structure specified from parsed JSON.  Each element in the JSON
        * structure is compared with the JSON to extract from.  Broadly speaking, elements whose
        * values are specified in the extractor must match, whereas variable elements appearing
        * in the extractor must exist. Lists may not appear in the extractor. */
      def unapplySeq(json: Json): Option[Seq[Json]] = try {
        var paths: List[SimplePath] = Nil
        def extract(struct: Any, path: SimplePath): Unit =
          struct match {
            case d: Double =>
              if(json.extract(path).get[Double](Extractor.doubleExtractor,
                  strategy.throwExceptions) != d) throw new Exception("Value doesn't match")
            case s: String =>
              if(json.extract(path).get[String](Extractor.stringExtractor,
                  strategy.throwExceptions) != s) throw new Exception("Value doesn't match")
            case m: Map[_, _] => m foreach {
              case (k, v) =>
                if(v == null) paths ::= (path / k.asInstanceOf[String])
                else extract(v, path / k.asInstanceOf[String])
            }
            case a: List[_] => ()
              // Emit an exception if attempting to extract on lists
          }
        extract(jp.parse(sc.parts.mkString("null")).get, ^)
        val extracts = paths.reverse.map(json.extract)
        if(extracts.exists(_.json == null)) None
        else Some(extracts map { x => new Json(x.normalize) })
      } catch { case e: Exception => None }
    }
  }

  object JsonBuffer {
    def parse(s: String)(implicit jp: JsonParser, eh: ExceptionHandler):
        eh.![ParseException, JsonBuffer] = eh.except {
      new JsonBuffer(try jp.parseMutable(s).get catch {
        case e: NoSuchElementException => throw new ParseException(s)
      })
    }

  }

  /** Companion object to the `Json` type, providing factory and extractor methods, and a JSON
    * pretty printer. */
  object Json {

    /** Parses a string containing JSON into a `Json` object */
    def parse(s: String)(implicit jp: JsonParser, eh: ExceptionHandler):
        eh.![ParseException, Json] = eh.except {
      new Json(try jp.parse(s).get catch {
        case e: NoSuchElementException => throw new ParseException(s)
      })
    }

    /** Wraps a map into a JSON object */
    def apply(map: Map[String, Any]): Json = new Json(map)

    /** Wraps a list into a JSON array */
    def apply(list: List[Any]): Json = new Json(list)

    def unapply(json: Any): Option[Json] = Some(new Json(json))

    def format(json: Json): String = format(Some(json.json), 0)

    /** Formats the JSON object for multi-line readability. */
    def format(json: Option[Any], ln: Int): String = {
      val indent = " "*ln
      json match {
        case Some(o: scala.collection.Map[_, _]) =>
          List("{", o.keys map { k => indent+" "+"\""+k+"\": "+format(o.get(k), ln + 1) } mkString
              ",\n", indent+"}").mkString("\n")
        case Some(a: Seq[_]) =>
          List("[", a map { v => indent+" "+format(Some(v), ln + 1) } mkString(",\n"),
              indent+"]") mkString "\n"
        case Some(s: String) =>
          "\""+s.replaceAll("\\\\", "\\\\\\\\").replaceAll("\r", "\\\\r").replaceAll("\n", "\\\\n").
              replaceAll("\"", "\\\\\"")+"\""
        case Some(n: Int) => n.toString
        case Some(n: Number) => n.toString
        case Some(v: Boolean) => if(v) "true" else "false"
        case Some(j: Json) => format(Some(j.json), ln)
        case Some(j: JsonBuffer) => format(Some(j.json), ln)
        case None => "null"
        case _ => "undefined"
      }
    }

    def serialize(json: Json): String = serialize(Some(json.normalize))

    def serialize(json: Option[Any]): String = {
      json match {
        case Some(o: scala.collection.Map[_, _]) =>
          List("{", o.keys map { k => "\""+k+"\":"+serialize(o.get(k)) } mkString
              ",", "}").mkString
        case Some(a: Seq[_]) =>
          List("[", a map { v => serialize(Some(v)) } mkString(","),
              "]") mkString ""
        case Some(s: String) =>
          "\""+s.replaceAll("\\\\", "\\\\\\\\").replaceAll("\n", "\\\\n").replaceAll("\"",
              "\\\\\"")+"\""
        case Some(n: Int) => n.toString
        case Some(n: Number) => n.toString
        case Some(v: Boolean) => if(v) "true" else "false"
        case Some(j: Json) => serialize(Some(j.json))
        case Some(j: JsonBuffer) => serialize(Some(j.json))
        case None => "null"
        case _ => "undefined"
      }
    }

  }

  /** Companion object for Extractor type. Defines very simple extractor methods for different
    * types which may be contained within. */
  object Extractor {

    implicit val noopExtractor = new Extractor[Json](x => new Json(x))
    implicit val noopExtractor2 = new Extractor[JsonBuffer](x => new JsonBuffer(x))
    implicit val stringExtractor = new Extractor[String](_.asInstanceOf[String])
    implicit val doubleExtractor = new Extractor[Double](_.asInstanceOf[Double])
    implicit val intExtractor = new Extractor[Int]({ x => try x.asInstanceOf[Int] catch {
        case e: ClassCastException => x.asInstanceOf[Double].toInt } })
    implicit val byteExtractor = new Extractor[Byte]({ x => try x.asInstanceOf[Int].toByte catch {
        case e: ClassCastException => x.asInstanceOf[Double].toByte } })
    implicit val longExtractor = new Extractor[Long](_.asInstanceOf[Double].toLong)
    implicit val booleanExtractor = new Extractor[Boolean](_.asInstanceOf[Boolean])
    implicit val anyExtractor = new Extractor[Any](identity)

    implicit def listExtractor[T: Extractor] =
      new Extractor[List[T]](_.asInstanceOf[Seq[Any]].to[List] map { x =>
        implicitly[Extractor[T]].cast(x)
      })

    implicit def optionExtractor[T: Extractor] =
      new Extractor[Option[T]](x => if(x == null) None else Some(x.asInstanceOf[Any]).map(
          implicitly[Extractor[T]].cast)) {
        override def errorToNull = true
      }

    implicit def mapExtractor[T: Extractor] =
      new Extractor[Map[String, T]](_.asInstanceOf[scala.collection.Map[String, Any]].
          toMap.mapValues(implicitly[Extractor[T]].cast))
  }

  @implicitNotFound("Cannot extract type ${T} from JSON.")
  class Extractor[T](val cast: Any => T) {
    def errorToNull = false
  }

  class JsonExtractor[T](cast: Json => T) extends Extractor[T](x => cast(new Json(x)))

  class Json(private[JsonProcessing] val json: Any, path: List[Either[Int, String]] = Nil)
      extends Dynamic {

    /** Assumes the Json object is wrapping a List, and extracts the `i`th element from the list */
    def apply(i: Int): Json =
      new Json(json, Left(i) :: path)

    /** Combines a `selectDynamic` and an `apply`.  This is necessary due to the way dynamic
      * application is expanded. */
    def applyDynamic(key: String)(i: Int): Json = selectDynamic(key).apply(i)

    /** Navigates the JSON using the `SimplePath` parameter, and returns the element at that
      * position in the tree. */
    def extract(sp: SimplePath): Json =
      if(sp == ^) this else selectDynamic(sp.head).extract(sp.tail)

    /** Assumes the Json object wraps a `Map`, and extracts the element `key`. */
    def selectDynamic(key: String): Json =
      new Json(json, Right(key) :: path)

    private[JsonProcessing] def normalize: Any = {
      yCombinator[(Any, List[Either[Int, String]]), Any] { fn => v => v match {
        case (j, Nil) => j
        case (j, Left(i) :: t) =>
          fn(try j.asInstanceOf[List[Any]](i) catch {
            case e: ClassCastException => throw MissingValueException()
            case e: IndexOutOfBoundsException => throw MissingValueException()
          }, t)
        case (j, Right(k) :: t) =>
          fn(try j.asInstanceOf[Map[String, Any]](k) catch {
            case e: ClassCastException => throw MissingValueException()
            case e: NoSuchElementException => throw MissingValueException()
          }, t)

      } } (json -> path.reverse)
    }

    /** @deprecated use 'to' */
    def get[T](implicit extractor: Extractor[T], eh: ExceptionHandler): eh.![JsonGetException, T] = to

    /** Assumes the Json object is wrapping a `T`, and casts (intelligently) to that type. */
    def to[T](implicit extractor: Extractor[T], eh: ExceptionHandler):
        eh.![JsonGetException, T] = eh.except(try extractor.cast(if(extractor.errorToNull)
            (try normalize catch { case e: Exception => null }) else normalize) catch {
          case e: MissingValueException => throw e
          case e: Exception => throw new TypeMismatchException()
        })

    /** Assumes the Json object is wrapping a List, and returns the length */
  /*  def length: Int = json match {
      case m1:scala.collection.immutable.Map[String,::[Any]] => m1.iterator.size
      case j:List[Json] => j.length
      case m2:scala.collection.immutable.Map2[String,::[Any]] => m2.iterator.size
      case m3:scala.collection.immutable.Map3[String,::[Any]] => m3.iterator.size
      case _ => _.iterator.size
    }*/

    /** Assumes the Json object is wrapping a List, and returns an iterator over the list */
   /* def iterator: Iterator[Any] = json match {
      case m3:scala.collection.immutable.Map.Map3[String,::[Any]] => m3.iterator
      case j:List[Json] => j.iterator
      case m1:scala.collection.immutable.Map.Map1[String,::[Any]] => m1.iterator
      case m2:scala.collection.immutable.Map.Map2[String,::[Any]] => m2.iterator
      case _ => _.iterator
    } */

    /** Assumes the Json object is wrapping a List, and returns the length */
   def length: Int = {
      try {
        json.asInstanceOf[List[Json]].length
      } catch {
        case ex:java.lang.ClassCastException =>
        try {
          json.asInstanceOf[scala.collection.immutable.Map.Map1[String,Json]].iterator.size
        } catch {
          case ex:java.lang.ClassCastException =>
          try {
            json.asInstanceOf[scala.collection.immutable.Map.Map2[String,Json]].iterator.size
          } catch {
            case ex:java.lang.ClassCastException =>
              json.asInstanceOf[scala.collection.immutable.Map.Map3[String,Json]].iterator.size
          }
        }
      }
    }

    /** Assumes the Json object is wrapping a List, and returns an iterator over the list */
     def iterator: Iterator[Any] = {
      try {
         json.asInstanceOf[List[Json]].iterator
      } catch {
        case ex:java.lang.ClassCastException =>
          try {
            json.asInstanceOf[scala.collection.immutable.Map.Map1[String,Json]].iterator
          } catch {
            case ex:java.lang.ClassCastException =>
              try {
                json.asInstanceOf[scala.collection.immutable.Map.Map2[String,Json]].iterator
              } catch {
                case ex:java.lang.ClassCastException =>
                  json.asInstanceOf[scala.collection.immutable.Map.Map3[String,Json]].iterator
              }
          }
      }
    }

    override def toString =
      try Json.format(Some(normalize), 0) catch {
        case e: ExceptionHandling#JsonGetException => "<error>"
      }
  }

  class JsonBuffer(private[JsonProcessing] val json: Any, path: List[Either[Int, String]] = Nil)
      extends Dynamic {
    /** Updates the element `key` of the JSON object with the value `v` */
    def updateDynamic(key: String)(v: Any): Unit =
      normalize(false, true).asInstanceOf[HashMap[String, Any]](key) = v

    /** Updates the `i`th element of the JSON array with the value `v` */
    def update(i: Int, v: Any): Unit = normalize(true, true).asInstanceOf[ListBuffer[Any]](i) = v

    /** Removes the specified key from the JSON object */
    def -=(k: String): Unit = normalize(false, true).asInstanceOf[HashMap[String, Any]].remove(k)

    /** Adds the specified value to the JSON array */
    def +=(v: Any): Unit = normalize(true, true).asInstanceOf[ListBuffer[Any]] += v

    /** Assumes the Json object is wrapping a ListBuffer, and extracts the `i`th element from the
      * list */
    def apply(i: Int): JsonBuffer =
      new JsonBuffer(json, Left(i) :: path)

    /** Combines a `selectDynamic` and an `apply`.  This is necessary due to the way dynamic
      * application is expanded. */
    def applyDynamic(key: String)(i: Int): JsonBuffer = selectDynamic(key).apply(i)

    /** Navigates the JSON using the `SimplePath` parameter, and returns the element at that
      * position in the tree. */
    def extract(sp: SimplePath): JsonBuffer =
      if(sp == ^) this else selectDynamic(sp.head).extract(sp.tail)

    /** Assumes the Json object wraps a `Map`, and extracts the element `key`. */
    def selectDynamic(key: String): JsonBuffer =
      new JsonBuffer(json, Right(key) :: path)

    private[JsonProcessing] def normalize(array: Boolean, modify: Boolean): Any = {
      yCombinator[(Any, List[Either[Int, String]]), Any] { fn => v => v match {
        case (j, Nil) => j
        case (j, Left(i) :: t) =>
          fn(try j.asInstanceOf[ListBuffer[Any]](i) catch {
            case e: ClassCastException => throw MissingValueException()
            case e: IndexOutOfBoundsException => throw MissingValueException()
          }, t)
        case (j, Right(k) :: t) =>
          val obj = if(array && t == Nil) new ListBuffer[Any] else new HashMap[String, Any]()
          fn(try {
            if(modify) j.asInstanceOf[HashMap[String, Any]].getOrElseUpdate(k, obj)
            else j.asInstanceOf[HashMap[String, Any]](k)
          } catch {
            case e: ClassCastException => throw MissingValueException()
            case e: NoSuchElementException => throw MissingValueException()
          }, t)

      } } (json -> path.reverse)
    }

    /** Assumes the Json object is wrapping a `T`, and casts (intelligently) to that type. */
    def get[T](implicit extractor: Extractor[T], eh: ExceptionHandler):
        eh.![JsonGetException, T] =
          eh.except(try extractor.cast(if(extractor.errorToNull)
              (try normalize(false, false) catch { case e: Exception => null }) else
              normalize(false, false)) catch {
            case e: MissingValueException => throw e
            case e: Exception => throw new TypeMismatchException()
          })

    /** Assumes the Json object is wrapping a List, and returns an iterator over the list */
    def iterator: Iterator[JsonBuffer] =
      normalize(true, false).asInstanceOf[ListBuffer[JsonBuffer]].iterator

    override def toString =
      try Json.format(Some(normalize(false, false)), 0) catch {
        case e: ExceptionHandling#JsonGetException => "<error>"
      }
  }
}

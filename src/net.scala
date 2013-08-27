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

import language.existentials

import java.io._
import java.net._

/** Provides functionality for handling internet URLs, namely HTTP and HTTPS schemes. */
trait Net extends Linking with JsonProcessing with MimeTyping with Services { this: Streaming =>

  object HttpMethods {
    
    private val methods = new scala.collection.mutable.HashMap[String, Method]
    
    sealed class Method(val string: String) {
      
      def unapply(r: String) = r == string
      override def toString = string
      
      methods += string -> this
    }

    trait FormMethod { this: Method => }

    def method(s: String) = methods(s)

    val Get = new Method("GET") with FormMethod
    val Put = new Method("PUT")
    val Post = new Method("POST") with FormMethod
    val Delete = new Method("DELETE")
    val Trace = new Method("TRACE")
    val Options = new Method("OPTIONS")
    val Head = new Method("HEAD")
    val Connect = new Method("CONNECT")
    val Patch = new Method("PATCH")

  }

  class HttpResponse(val headers: Map[String, List[String]], val status: Int, is: InputStream) {
    def input[Data](implicit ib: InputBuilder[InputStream, Data], eh: ExceptionHandler):
        eh.![Exception, Input[Data]]=
      eh.except(ib.input(is))
  }

  trait PostType[-C] {
    def contentType: Option[MimeTypes.MimeType]
    def sender(content: C): Input[Byte]
  }

  implicit val FormPostType = new PostType[Map[Symbol, String]] {
    def contentType = Some(MimeTypes.`application/x-www-form-urlencoded`)
    def sender(content: Map[Symbol, String]) = ByteArrayInput((content map { case (k, v) =>
      URLEncoder.encode(k.name, "UTF-8")+"="+URLEncoder.encode(v, "UTF-8")
    } mkString "&").getBytes("UTF-8"))
  }

  implicit val StringPostType = new PostType[String] {
    def contentType = Some(MimeTypes.`text/plain`)
    def sender(content: String) = ByteArrayInput(content.getBytes("UTF-8"))
  }

  implicit val NonePostType = new PostType[None.type] {
    def contentType = Some(MimeTypes.`application/x-www-form-urlencoded`)
    def sender(content: None.type) = ByteArrayInput(Array[Byte](0))
  }

  implicit val JsonPostType = new PostType[Json] {
    def contentType = Some(MimeTypes.`application/x-www-form-urlencoded`)
    def sender(content: Json) =
      ByteArrayInput(content.toString.getBytes("UTF-8"))
  }

  /** Common methods for `HttpUrl`s */
  trait NetUrl extends Url[NetUrl] with Uri {
    
    import javax.net.ssl._
    import javax.security.cert._
    
    private[rapture] def javaConnection: HttpURLConnection =
      new URL(toString).openConnection().asInstanceOf[HttpURLConnection]

    private val trustAllCertificates = {
      Array[TrustManager](new X509TrustManager {
        override def getAcceptedIssuers(): Array[java.security.cert.X509Certificate] = null
        
        def checkClientTrusted(certs: Array[java.security.cert.X509Certificate], authType: String):
            Unit = ()
        
        def checkServerTrusted(certs: Array[java.security.cert.X509Certificate], authType: String):
            Unit = ()
      })
    }
    
    private val sslContext = SSLContext.getInstance("SSL")
    sslContext.init(null, trustAllCertificates, new java.security.SecureRandom())

    private val allHostsValid = new HostnameVerifier {
      def verify(hostname: String, session: SSLSession) = true
    }

    def hostname: String
    def port: Int
    def ssl: Boolean
    def canonicalPort: Int

    private val base64 = new Base64Codec(endPadding = true)

    def schemeSpecificPart = "//"+hostname+(if(port == canonicalPort) "" else ":"+port)+pathString

    /** Sends an HTTP put to this URL.
      *
      * @param content the content to put to the URL
      * @param authenticate the username and password to provide for basic HTTP authentication,
      *        defaulting to no authentication.
      * @return the HTTP response from the remote host */
    def put[C: PostType](content: C, authenticate: Option[(String, String)] = None,
        ignoreInvalidCertificates: Boolean = false, httpHeaders: Map[String, String] =
        Map(), followRedirects: Boolean = true)(implicit eh: ExceptionHandler): eh.![HttpExceptions, HttpResponse] =
      post(content, authenticate, ignoreInvalidCertificates, httpHeaders, "PUT", followRedirects)(
          implicitly[PostType[C]], eh)
  
    def get(authenticate: Option[(String, String)] = None, ignoreInvalidCertificates: Boolean =
        false, httpHeaders: Map[String, String] = Map(), followRedirects: Boolean = true)(implicit eh: ExceptionHandler):
        eh.![HttpExceptions, HttpResponse] = post(None, authenticate, ignoreInvalidCertificates,
        httpHeaders, "GET", followRedirects)(implicitly[PostType[None.type]], eh)
        

    /** Sends an HTTP post to this URL.
      *
      * @param content the content to post to the URL
      * @param authenticate the username and password to provide for basic HTTP authentication,
      *        defaulting to no authentication.
      * @return the HTTP response from the remote host */
    def post[C: PostType](content: C, authenticate: Option[(String, String)] = None,
        ignoreInvalidCertificates: Boolean = false, httpHeaders: Map[String, String] = Map(),
        method: String = "POST", followRedirects: Boolean = true)(implicit eh: ExceptionHandler):
        eh.![HttpExceptions, HttpResponse] = eh.except {

      // FIXME: This will produce a race condition if creating multiple URL connections with
      // different values for followRedirects in parallel
      HttpURLConnection.setFollowRedirects(followRedirects)
      val conn: URLConnection = new URL(toString).openConnection()
      conn match {
        case c: HttpsURLConnection =>
          if(ignoreInvalidCertificates) {
            c.setSSLSocketFactory(sslContext.getSocketFactory)
            c.setHostnameVerifier(allHostsValid)
          }
          c.setRequestMethod(method)
          if(content != None) c.setDoOutput(true)
          c.setUseCaches(false)
        case c: HttpURLConnection =>
          c.setRequestMethod(method)
          if(content != None) c.setDoOutput(true)
          c.setUseCaches(false)
      }

      if(authenticate.isDefined) conn.setRequestProperty("Authorization",
          "Basic "+base64.encode((authenticate.get._1+":"+authenticate.get._2).getBytes("UTF-8")).mkString)

      implicitly[PostType[C]].contentType map { ct =>
        conn.setRequestProperty("Content-Type", ct.name)
      }
      for((k, v) <- httpHeaders) conn.setRequestProperty(k, v)

      if(content != None)
        ensuring(OutputStreamBuilder.output(conn.getOutputStream)) { out =>
          implicitly[PostType[C]].sender(content) > out
        } (_.close())

      import scala.collection.JavaConversions._

      val statusCode = conn match {
        case c: HttpsURLConnection => c.getResponseCode()
        case c: HttpURLConnection => c.getResponseCode()
      }
      
      val is = try conn.getInputStream() catch {
        case e: IOException => conn match {
          case c: HttpsURLConnection => c.getErrorStream()
          case c: HttpURLConnection => c.getErrorStream()
        }
      }
      new HttpResponse(mapAsScalaMap(conn.getHeaderFields()).toMap.mapValues(_.to[List]),
          statusCode, is)
    }
  }

  class HttpQueryParametersBase[U, T <: Iterable[U]] extends QueryType[Path[_], T] {
    def extras(existing: AfterPath, q: T): AfterPath =
      existing + ('?' -> ((q.map({ case (k: Symbol, v: String) =>
        UrlCodec(k.name).urlEncode+"="+UrlCodec(v).urlEncode
      }).mkString("&")) -> 1.0))
  }

  implicit object HttpQueryParametersMap extends HttpQueryParametersBase[(Symbol, String), Map[Symbol, String]]

  implicit object HttpQueryParametersIter extends HttpQueryParametersBase[(Symbol, String), Seq[(Symbol, String)]]

  implicit object PageIdentifier extends QueryType[Path[_], Symbol] {
    def extras(existing: AfterPath, q: Symbol): AfterPath =
      existing + ('#' -> (q.name -> 2.0))
  }

  /** Represets a URL with the http scheme */
  class HttpUrl(val pathRoot: NetPathRoot[HttpUrl], elements: Seq[String], afterPath: AfterPath,
      val ssl: Boolean) extends Url[HttpUrl](elements, afterPath) with NetUrl with
      PathUrl[HttpUrl] { thisHttpUrl =>
    
    def makePath(ascent: Int, xs: Seq[String], afterPath: AfterPath) =
      new HttpUrl(pathRoot, elements, afterPath, ssl)
    
    def hostname = pathRoot.hostname
    def port = pathRoot.port
    def canonicalPort = if(ssl) 443 else 80
  }

  trait NetPathRoot[+T <: Url[T] with NetUrl] extends PathRoot[T] {
    def hostname: String
    def port: Int
  }

  class HttpPathRoot(val hostname: String, val port: Int, val ssl: Boolean) extends
      NetPathRoot[HttpUrl] with Uri { thisPathRoot =>
    
    def makePath(ascent: Int, elements: Seq[String], afterPath: AfterPath): HttpUrl =
      new HttpUrl(thisPathRoot, elements, Map(), ssl)

    def scheme = if(ssl) Https else Http
    def canonicalPort = if(ssl) 443 else 80
    def schemeSpecificPart = "//"+hostname+(if(port == canonicalPort) "" else ":"+port)+pathString

    override def /(element: String) = makePath(0, Array(element), Map())
    
    def /[P <: Path[P]](path: P) = makePath(0, path.elements, Map())
    
    override def equals(that: Any): Boolean =
      that.isInstanceOf[HttpPathRoot] && hostname == that.asInstanceOf[HttpPathRoot].hostname
  }

  /** Factory for creating new HTTP URLs */
  object Http extends Scheme[HttpUrl] {
    def schemeName = "http"
    
    /** Creates a new URL with the http scheme with the specified domain name and port
      *
      * @param hostname A `String` of the domain name for the URL
      * @param port The port to connect to this URL on, defaulting to port 80 */
    def /(hostname: String, port: Int = Services.Tcp.http.portNo) =
      new HttpPathRoot(hostname, port, false)
  
    private val UrlRegex = """(https?):\/\/([\.\-a-z0-9]+)(:[1-9][0-9]*)?\/?(.*)""".r

    /** Parses a URL string into an HttpUrl */
    def parse(s: String)(implicit eh: ExceptionHandler): eh.![ParseException, HttpUrl] =
        eh.except { s match {
      case UrlRegex(scheme, server, port, path) =>
        val rp = new SimplePath(path.split("/"), Map())
        scheme match {
          case "http" => Http./(server, if(port == null) 80 else port.substring(1).toInt) / rp
          case "https" => Https./(server, if(port == null) 443 else port.substring(1).toInt) / rp
          case _ => throw ParseException(s)
        }
      case _ => throw ParseException(s)
    } }
  }

  /** Factory for creating new HTTPS URLs */
  object Https extends Scheme[HttpUrl] {
    def schemeName = "https"

    /** Creates a new URL with the https scheme with the specified domain name and port
      *
      * @param hostname A `String` of the domain name for the URL
      * @param port The port to connect to this URL on, defaulting to port 443 */
    def /(hostname: String, port: Int = Services.Tcp.https.portNo) =
      new HttpPathRoot(hostname, port, true)
  }

  implicit object HttpUrlLinkable extends Linkable[HttpUrl, HttpUrl] {
    type Result = Link
    def link(src: HttpUrl, dest: HttpUrl) = {
      if(src.ssl == dest.ssl && src.hostname == dest.hostname && src.port == dest.port) {
        val lnk = generalLink(src.elements.to[List], dest.elements.to[List])
        new RelativePath(lnk._1, lnk._2, dest.afterPath)
      } else dest
    }
  }

}


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

import java.io._
import java.net._

/** Provides classes for creating and listening on sockets.  This code is largely incomplete. */
trait TcpHandling extends ExceptionHandling with Services with Slurping with Executing {

  /** Listens for incoming connections on the specified port
    *
    * @usecase def listen(port: Int): Input[Byte]
    * @param port the port to listen to */
  def listen[K](port: Int)(implicit ib: InputBuilder[InputStream, K],
      ob: OutputBuilder[OutputStream, K], eh: ExceptionHandler):
      eh.![Exception, (Input[K], Output[K])] = eh.except {
    val sock = new ServerSocket(port)
    val sock2 = sock.accept()
    (ib.input(sock2.getInputStream), ob.output(sock2.getOutputStream))
  }

  def tcpHandle[K](port: Int)(action: (Input[K], Output[K]) => Unit)
      (implicit ib: InputBuilder[InputStream, K], ob: OutputBuilder[OutputStream, K]): Unit = {
    val sock = new ServerSocket(port)
    while(true) {
      val sock2 = sock.accept()
      fork { action(ib.input(sock2.getInputStream), ob.output(sock2.getOutputStream)) }
    }
  }

  /*def listen(port: Int, local: Boolean = true, timeout: Int = 2000) = {
    val socket = new ServerSocket()
    socket.setSoTimeout(timeout)
    if(local) socket.bind(new InetSocketAddress("127.0.0.1", port))
    else socket.bind(new InetSocketAddress(port))
  }*/

  class SocketUri(val hostname: String, val port: Int) extends Uri {
    
    def scheme = Socket
    lazy val javaSocket: java.net.Socket = new java.net.Socket(hostname, port)
    
    def schemeSpecificPart = "//"+hostname+":"+port
    
    def absolute = true
 
  }

  object Socket extends Scheme[SocketUri] {
    def schemeName = "socket"
    def apply(hostname: String, port: Int): SocketUri = new SocketUri(hostname, port)
    def apply(hostname: String, svc: TcpService): SocketUri = new SocketUri(hostname, svc.portNo)
  }

  implicit object SocketStreamByteReader extends
      JavaInputStreamReader[SocketUri](_.javaSocket.getInputStream)
  
  implicit object SocketStreamByteWriter extends
      JavaOutputStreamWriter[SocketUri](_.javaSocket.getOutputStream)
  
}

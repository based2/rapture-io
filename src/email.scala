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

import javax.mail._
import javax.mail.internet._
import javax.activation._

trait Emailing extends Linking with MimeTyping {

  object Mailto extends Scheme[MailtoUri] {
    def schemeName = "mailto"
    def /(email: String): MailtoUri = new MailtoUri(email)
  }

  class MailtoUri(val email: String) extends Uri {
    def absolute = true
    def scheme = Mailto
    def schemeName = scheme.schemeName
    def schemeSpecificPart = email
  }

  case class EmailAddress(email: String, name: String = "") {
    override def toString = if(name == "") email else s""""${name}" <${email}>"""
  }

  object Smtp extends Scheme[Smtp] {
    def /(hostname: String) = new Smtp(hostname)
    def schemeName = "smtp"
  }

  trait Mailable[T] { def content(t: T): String }

  implicit def stringToEmail(s: String): EmailAddress = EmailAddress(s)

  implicit object StringMailable extends Mailable[String] {
    def content(t: String) = t
  }

  trait AddressedMailable[T] extends Mailable[T] {
    def sender(t: T): EmailAddress
    def recipients(t: T): List[EmailAddress]
    def ccRecipients(t: T): List[EmailAddress] = Nil
    def subject(t: T): String
  }

  class Smtp(val hostname: String) extends Uri {
    def scheme = Smtp
    def schemeSpecificPart = s"//${hostname}"
    def absolute = true

    def sendTo[Mail: Mailable](sender: EmailAddress, recipients: Seq[EmailAddress],
        ccRecipients: Seq[EmailAddress] = Nil, subject: String, mail: Mail): Unit = {
      sendmail(sender.toString, recipients.map(_.toString), ccRecipients.map(_.toString), subject,
          implicitly[Mailable[Mail]].content(mail), None, Nil)
    }

    def send[Mail: AddressedMailable](mail: Mail) = {
      val am = implicitly[AddressedMailable[Mail]]
      sendmail(am.sender(mail).toString, am.recipients(mail).map(_.toString),
          am.ccRecipients(mail).map(_.toString), am.subject(mail), am.content(mail), None, Nil)
    }

    def sendmail(from: String, to: Seq[String], cc: Seq[String], subject: String,
        bodyText: String, bodyHtml: Option[(String, Seq[(String, String)])],
        attachments: Seq[(String, String, String)]): Unit = {
    
      val props = System.getProperties()
      props.put("mail.smtp.host", hostname)
      val session = Session.getDefaultInstance(props, null)
      val msg = new MimeMessage(session)
      msg.setFrom(new InternetAddress(from))
      for(r <- to) msg.addRecipient(Message.RecipientType.TO, new InternetAddress(r))
      for(r <- cc) msg.addRecipient(Message.RecipientType.CC, new InternetAddress(r))
      msg.setSubject(subject)

      bodyHtml match {
        case Some(Pair(html, inlines)) => {
          var top = new MimeMultipart("alternative")
          val textPart = new MimeBodyPart()
          textPart.setText(bodyText, "UTF-8")
          top.addBodyPart(textPart)

          val htmlPart = new MimeBodyPart()
          htmlPart.setContent(html, "text/html;charset=UTF-8")
          top.addBodyPart(htmlPart)

          if(inlines.length > 0) {
            val body = new MimeBodyPart()
            body.setContent(top)
            top = new MimeMultipart("related")
            top.addBodyPart(body)
            for(i <- inlines) {
              val relPart = new MimeBodyPart()
              relPart.setDisposition(Part.INLINE)
              relPart.setHeader("Content-ID", "<"+i._1+">")
              val ds = new FileDataSource(i._2)
              ds.setFileTypeMap(MimeTypes.mimeTypesMap)
              relPart.setDataHandler(new DataHandler(ds))
              top.addBodyPart(relPart)
            }
          }

          if(attachments.length > 0) {
            val body = new MimeBodyPart()
            body.setContent(top)
            top = new MimeMultipart("mixed")
            top.addBodyPart(body)
            for(a <- attachments) {
              val attPart = new MimeBodyPart()
              attPart.setDisposition(Part.ATTACHMENT)
              attPart.setFileName(a._1)
              val src = new FileDataSource(a._3) {
                override def getContentType() = a._2
              }
              attPart.setDataHandler(new DataHandler(src))
              top.addBodyPart(attPart)
            }
          }
          msg.setContent(top)
        }
        case None => {
          if(attachments.length > 0) {
            val body = new MimeBodyPart()
            body.setText(bodyText, "UTF-8")
            val top = new MimeMultipart("mixed")
            top.addBodyPart(body)
            for(a <- attachments) {
              val attPart = new MimeBodyPart()
              attPart.setDisposition(Part.ATTACHMENT)
              attPart.setFileName(a._1)
              val src = new FileDataSource(a._3) {
                override def getContentType() = a._2
              }
              attPart.setDataHandler(new DataHandler(src))
              top.addBodyPart(attPart)
            }
            msg.setContent(top)
          } else {
            msg.setText(bodyText, "UTF-8")
          }
        }
      }
      Transport.send(msg)
    }
  }
}



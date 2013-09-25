package nz.org.sesa.los.server

import play.api.mvc._

package object util {
    def getBasicAuth[T](request : Request[T]) : Option[(String, String)] = {
        for {
            auth <- request.headers.get("Authorization")
        } yield {
            val Array(user, pass) = new String(new sun.misc.BASE64Decoder()
                .decodeBuffer(auth.replaceFirst("Basic ", "")), "UTF-8").split(":")
            (user, pass)
        }
    }
}

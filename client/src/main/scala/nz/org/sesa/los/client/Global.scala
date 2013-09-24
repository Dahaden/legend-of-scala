package nz.org.sesa.los.client

import dispatch._, Defaults._

object Global {
    val ServerAddress = {
        if (System.getenv("LOS_HOST") == null) {
            throw new RuntimeException("LOS_HOST not set.")
        }

        System.getenv("LOS_HOST")
    }

    val http = new Http()
}

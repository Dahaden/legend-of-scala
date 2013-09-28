import nz.org.sesa.los.client.Adventurer
import nz.org.sesa.los.client
import nz.org.sesa.los.client.items
import nz.org.sesa.los.client.util
import util.Display

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.ILoop

package object los {
    private class LosILoop extends ILoop {
        addThunk {
            enablePowerMode(true)
            intp.quietImport("los._")
            intp.quietRun("vals.isettings.maxPrintString = 0")
        }

        override def prompt = s"${Display.fg(196)}scala>${Display.Reset} "
    }

    val login = Adventurer.login(_, _)

    val Markers = util.Markers
    type Item = client.Item
    type Tile = items.Map.Tile
    type Signal = items.Beacon.Signal
    val Signal = items.Beacon.Signal
    type Position = client.Position

    def main(args: Array[String]) {
        if (System.getenv("LOS_HOST") == null) {
            println("LOS_HOST not set.")
            return
        }

        val settings = new Settings()
        settings.embeddedDefaults[Adventurer]

        val iloop = new LosILoop()
        iloop.process(settings)
    }
}

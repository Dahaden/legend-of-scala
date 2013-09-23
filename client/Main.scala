import nz.org.sesa.los.client.Adventurer
import nz.org.sesa.los.client
import nz.org.sesa.los.client.items
import nz.org.sesa.los.client.util

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.ILoop

package object los {
    private class LosILoop extends ILoop {
        addThunk {
            intp.quietImport("los._")
        }
    }

    val login = Adventurer.login(_)

    val Markers = util.Markers
    val Features = util.Features
    type Item = client.Item
    type Tile = items.Map.Tile
    type Signal = items.Beacon.Signal
    val Signal = items.Beacon.Signal

    def main(args: Array[String]) {
        val settings = new Settings()
        settings.embeddedDefaults[Adventurer]

        val iloop = new LosILoop()
        iloop.process(settings)
    }
}

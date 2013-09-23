import nz.org.sesa.los.client.Adventurer
import nz.org.sesa.los.client
import nz.org.sesa.los.client.items
import nz.org.sesa.los.client.util

package object los {
    val login = Adventurer.login(_)

    val Markers = util.Markers
    val Features = util.Features
    type Item = client.Item
    type Tile = items.PaperMap.Tile
    type Signal = items.Beacon.Signal
    val Signal = items.Beacon.Signal
}

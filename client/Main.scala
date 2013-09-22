import nz.org.sesa.los.client.Player
import nz.org.sesa.los.client.items
import nz.org.sesa.los.client.util

package object los {
    val login = Player.login(_)

    val Markers = util.Markers
    val Features = util.Features
    type Tile = items.Tile
    type Coordinate = (Int, Int)
}

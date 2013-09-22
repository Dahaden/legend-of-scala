#!/usr/bin/env python2

from __future__ import division

import sys
from lxml import etree

if len(sys.argv) != 2:
    print >>sys.stderr, "usage: {} <mapgen2 xml file>".format(sys.argv[0])
    exit(1)

BIOME_MAP = [
    'BEACH', 'LAKE', 'ICE', 'MARSH', 'SNOW', 'TUNDRA', 'BARE', 'SCORCHED',
    'TAIGA', 'SHRUBLAND', 'TEMPERATE_DESERT', 'TEMPERATE_RAIN_FOREST',
    'TEMPERATE_DECIDUOUS_FOREST', 'GRASSLAND', 'SUBTROPICAL_DESERT',
    'TROPICAL_RAIN_FOREST', 'TROPICAL_SEASONAL_FOREST'
]

COLORS = {
    'OCEAN': 0x44447a,
    'COAST': 0x33335a,
    'LAKESHORE': 0x225588,
    'LAKE': 0x336699,
    'RIVER': 0x225588,
    'MARSH': 0x2f6666,
    'ICE': 0x99ffff,
    'BEACH': 0xa09077,
    'ROAD1': 0x442211,
    'ROAD2': 0x553322,
    'ROAD3': 0x664433,
    'BRIDGE': 0x686860,
    'LAVA': 0xcc3333,

    # Terrain
    'SNOW': 0xffffff,
    'TUNDRA': 0xbbbbaa,
    'BARE': 0x888888,
    'SCORCHED': 0x555555,
    'TAIGA': 0x99aa77,
    'SHRUBLAND': 0x889977,
    'TEMPERATE_DESERT': 0xc9d29b,
    'TEMPERATE_RAIN_FOREST': 0x448855,
    'TEMPERATE_DECIDUOUS_FOREST': 0x679459,
    'GRASSLAND': 0x88aa55,
    'SUBTROPICAL_DESERT': 0xd2b98b,
    'TROPICAL_RAIN_FOREST': 0x337755,
    'TROPICAL_SEASONAL_FOREST': 0x559944
}

for c in COLORS.iterkeys():
    COLORS[c] = ((COLORS[c] & 0xff0000) >> 16,
                 (COLORS[c] & 0x00ff00) >> 8,
                 COLORS[c] & 0x0000ff)

REVERSE_COLORS = dict((v, k.lower().replace('_', '-')) for k, v in COLORS.iteritems())

ZOOM_FACTOR = 0.25


class Map(object):
    def __init__(self, tree):
        self.tree = tree

        self.centers = {}
        self.corners = {}
        self.edges = {}

        self.roads = set([])

    def populate(self):
        for center_elem in self.tree.find('centers'):
            self.centers[int(center_elem.attrib['id'])] = Center(center_elem)

        for corner_elem in self.tree.find('corners'):
            self.corners[int(corner_elem.attrib['id'])] = Corner(corner_elem)

        for edge_elem in self.tree.find('edges'):
            self.edges[int(edge_elem.attrib['id'])] = Edge(edge_elem)

        for road_elem in self.tree.find('roads'):
            self.edges[int(road_elem.attrib['edge'])].road = int(road_elem.attrib['contour'])

        for id, center in self.centers.iteritems():
            center.populate(self)

        for id, edge in self.edges.iteritems():
            edge.populate(self)


class Center(object):
    def __init__(self, elem):
        self.elem = elem

        self.id = int(elem.attrib['id'])
        self.x = float(elem.attrib['x'])
        self.y = float(elem.attrib['y'])
        self.water = elem.attrib['water'] == 'true'
        self.ocean = elem.attrib['ocean'] == 'true'
        self.coast = elem.attrib['coast'] == 'true'
        self.border = elem.attrib['border'] == 'true'
        self.biome = elem.attrib['biome']
        self.elevation = float(elem.attrib['elevation'])
        self.moisture = float(elem.attrib['moisture'])

        self.neighbors = []
        self.borders = []
        self.corners = []

    def populate(self, map):
        for neighbor_elem in self.elem.findall('center'):
            self.neighbors.append(map.centers[int(neighbor_elem.attrib['id'])])

        for corner_elem in self.elem.findall('corner'):
            self.corners.append(map.corners[int(corner_elem.attrib['id'])])

        for border_elem in self.elem.findall('edge'):
            self.borders.append(map.edges[int(border_elem.attrib['id'])])

    @property
    def point(self):
        return self.x, self.y

    def __repr__(self):
        return ("Center(id={id}, x={x}, y={y}, water={water}, ocean={ocean}, "
                "border={border}, biome={biome}, elevation={elevation}, "
                "moisture={moisture})").format(**self.__dict__)


class Corner(object):
    def __init__(self, elem):
        self.elem = elem

        self.id = int(elem.attrib['id'])
        self.x = float(elem.attrib['x'])
        self.y = float(elem.attrib['y'])
        self.water = elem.attrib['water'] == 'true'
        self.ocean = elem.attrib['ocean'] == 'true'
        self.coast = elem.attrib['coast'] == 'true'
        self.border = elem.attrib['border'] == 'true'
        self.elevation = float(elem.attrib['elevation'])
        self.moisture = float(elem.attrib['moisture'])
        self.river = int(elem.attrib['river'])

    @property
    def point(self):
        return self.x, self.y

    def __repr__(self):
        return ("Corner(id={id}, x={x}, y={y}, water={water}, ocean={ocean}, "
                "border={border}, elevation={elevation}, moisture={moisture}, "
                "river={river})").format(**self.__dict__)


class Edge(object):
    def __init__(self, elem):
        self.elem = elem

        self.id = int(elem.attrib['id'])
        self.river = int(elem.attrib['river'])
        self.road = 0

    def populate(self, map):
        self.center0 = map.centers[int(self.elem.attrib['center0'])] if 'center0' in self.elem.attrib else None
        self.center1 = map.centers[int(self.elem.attrib['center1'])] if 'center1' in self.elem.attrib else None

        self.corner0 = map.corners[int(self.elem.attrib['corner0'])] if 'corner0' in self.elem.attrib else None
        self.corner1 = map.corners[int(self.elem.attrib['corner1'])] if 'corner1' in self.elem.attrib else None

    @property
    def midpoint(self):
        return ((self.corner0.x + self.corner1.x) / 2,
                (self.corner0.y + self.corner1.y) / 2)

    @property
    def connections(self):
        for edge in self.center0.borders:
            if edge is self:
                continue
            yield edge

        for edge in self.center1.borders:
            if edge is self:
                continue
            yield edge

    def __repr__(self):
        return ("Edge(id={id}, x={x}, y={y}, river={river})").format(**self.__dict__)

if len(sys.argv) != 2:
    print >>sys.stderr, "usage: {} <mapgen2 xml>".format(sys.argv[0])
    exit(1)

print >>sys.stderr, "parsing map..."
map = Map(etree.parse(sys.argv[1]))
map.populate()

import math
import json
import Image
import ImageDraw

SIZE = 600
img = Image.new("RGB", (int(SIZE * ZOOM_FACTOR), int(SIZE * ZOOM_FACTOR)))

ctx = ImageDraw.Draw(img)
ctx.polygon([
    (0, 0),
    (0, SIZE * ZOOM_FACTOR),
    (SIZE * ZOOM_FACTOR, SIZE * ZOOM_FACTOR),
    (SIZE * ZOOM_FACTOR, 0),
], fill=COLORS['OCEAN'])


def vec2_length(x, y):
    return math.sqrt(x * x + y * y)


def clockwise_cmp_factory(center):
    cx, cy = center

    def clockwise_cmp(a, b):
        if a == b:
            return 0

        ax, ay = a
        bx, by = b

        angle1 = math.atan2(ay - cy, ax - cx)
        angle2 = math.atan2(by - cy, bx - cx)

        return cmp(angle1, angle2)

    return clockwise_cmp

print >>sys.stderr, "filling polygons..."
for center in map.centers.itervalues():
    if center.biome == 'OCEAN':
        continue

    points = []
    for corner in center.corners:
        points.append((corner.x * ZOOM_FACTOR, corner.y * ZOOM_FACTOR))
    points.sort(clockwise_cmp_factory((center.x * ZOOM_FACTOR, center.y * ZOOM_FACTOR)))

    color = COLORS[center.biome]

    ctx.polygon(points, fill=color)


print >>sys.stderr, "stroking rivers..."
for edge in map.edges.itervalues():
    # draw rivers with non-magical code
    if edge.river:
        if edge.center0.water or edge.center1.water:
            continue

        ctx.line([
            (edge.corner0.x * ZOOM_FACTOR, edge.corner0.y * ZOOM_FACTOR),
            (edge.corner1.x * ZOOM_FACTOR, edge.corner1.y * ZOOM_FACTOR)
        ], fill=COLORS['RIVER'], width=int(edge.river / 1.5 * ZOOM_FACTOR))


# draw roads with magical algorithm stol--err, borrowed from the AS3 code
# https://github.com/amitp/mapgen2/blob/master/mapgen2.as#L657

# calculate the points for quadratic bezier
def quadratic_bezier_points(a, b, c, t):
    ax, ay = a
    bx, by = b
    cx, cy = c

    u = 1 - t

    return (
        u * (u * ax + t * bx) + t * (u * bx + t * cx),
        u * (u * ay + t * by) + t * (u * by + t * cy)
    )


# Helper function: find the normal vector across edge 'e' and
# make sure to point it in a direction towards 'c'.
def normal_towards(ev0, ev1, midpoint, c, len):
    ev0x, ev0y = ev0
    ev1x, ev1y = ev1
    mx, my = midpoint
    cx, cy = c

    nx = -(ev1y - ev0y)
    ny = +(ev1x - ev1x)

    dx = cx - mx
    dy = cy - my

    if nx * dx + ny * dy < 0:
        nx = -nx
        ny = -ny

    nmag = vec2_length(nx, ny)
    return nx / (nmag * len), ny / (nmag * len)


print >>sys.stderr, "stroking roads..."
for edge in map.edges.itervalues():
    if edge.road:
        for p in [edge.center0, edge.center1]:
            roadConnections = [e for e in edge.connections if e.road]
            roadEdges = [e for e in p.borders if e.road]

            if len(roadConnections) == 2:
                # Regular road: draw a spline from one edge to the other.

                for edge1 in roadEdges:
                    for edge2 in roadEdges:
                        d = 0.5 * min(
                            vec2_length(
                                edge1.midpoint[0] - p.x,
                                edge1.midpoint[1] - p.y
                            ),
                            vec2_length(
                                edge2.midpoint[0] - p.x,
                                edge2.midpoint[1] - p.y
                            )
                        )

                        ax, ay = normal_towards(
                            edge1.center0.point,
                            edge1.center1.point,
                            edge1.midpoint,
                            p.point,
                            d
                        )
                        ax += edge1.midpoint[0]
                        ay += edge1.midpoint[1]

                        bx, by = normal_towards(
                            edge2.center0.point,
                            edge2.center1.point,
                            edge2.midpoint,
                            p.point,
                            d
                        )
                        bx += edge2.midpoint[0]
                        by += edge2.midpoint[1]

                        cx = ax + (bx - ax) * 0.5
                        cy = ay + (by - ay) * 0.5

                        for i in xrange(10):
                            t = i / 10.
                            u = (i + 1) / 10.

                            tx, ty = quadratic_bezier_points(edge1.midpoint, (ax, ay), (cx, cy), t)
                            ux, uy = quadratic_bezier_points(edge1.midpoint, (ax, ay), (cx, cy), u)

                            ctx.line([
                                (tx * ZOOM_FACTOR, ty * ZOOM_FACTOR),
                                (ux * ZOOM_FACTOR, uy * ZOOM_FACTOR)
                            ], fill=COLORS['ROAD{}'.format(edge.road)], width=int(ZOOM_FACTOR / 1.5))

                            tx, ty = quadratic_bezier_points((cx, cy), (bx, by), edge2.midpoint, t)
                            ux, uy = quadratic_bezier_points((cx, cy), (bx, by), edge2.midpoint, u)

                            ctx.line([
                                (tx * ZOOM_FACTOR, ty * ZOOM_FACTOR),
                                (ux * ZOOM_FACTOR, uy * ZOOM_FACTOR)
                            ], fill=COLORS['ROAD{}'.format(edge.road)], width=int(ZOOM_FACTOR / 1.5))
            else:
                for edge1 in roadEdges:
                    d = 0.25 * vec2_length(edge1.midpoint[0] - p.x, edge1.midpoint[1] - p.y)

                    ax, ay = normal_towards(
                        edge1.center0.point,
                        edge1.center1.point,
                        edge1.midpoint,
                        p.point,
                        d
                    )
                    ax += edge1.midpoint[0]
                    ay += edge1.midpoint[1]

                    for i in xrange(10):
                        t = i / 10.
                        u = (i + 1) / 10.

                        tx, ty = quadratic_bezier_points(edge1.midpoint, (ax, ay), (p.x, p.y), t)
                        ux, uy = quadratic_bezier_points(edge1.midpoint, (ax, ay), (p.x, p.y), u)

                        ctx.line([
                            (tx * ZOOM_FACTOR, ty * ZOOM_FACTOR),
                            (ux * ZOOM_FACTOR, uy * ZOOM_FACTOR)
                        ], fill=COLORS['ROAD{}'.format(edge.road)], width=int(ZOOM_FACTOR / 1.5))

img_data = img.load()
img_data_in = img.copy().load()

# rectilinearize roads and rivers

print >>sys.stderr, "rectilinearizing roads and rivers..."
for j in xrange(img.size[1]):
    for i in xrange(img.size[0]):
        pixel = img_data_in[i, j]

        if pixel == COLORS['RIVER']:
            if img_data_in[i - 1, j - 1] == pixel:
                img_data[i, j - 1] = pixel

            if img_data_in[i + 1, j - 1] == pixel:
                img_data[i + 1, j] = pixel

            if img_data_in[i + 1, j + 1] == pixel:
                img_data[i, j + 1] = pixel

            if img_data_in[i - 1, j + 1] == pixel:
                img_data[i - 1, j] = pixel

        if pixel in (COLORS['ROAD1'], COLORS['ROAD2'],
                     COLORS['ROAD3']):
            if img_data_in[i - 1, j - 1] == pixel:
                img_data[i, j - 1] = pixel

            if img_data_in[i + 1, j - 1] == pixel:
                img_data[i + 1, j] = pixel

            if img_data_in[i + 1, j + 1] == pixel:
                img_data[i, j + 1] = pixel

            if img_data_in[i - 1, j + 1] == pixel:
                img_data[i - 1, j] = pixel

print >>sys.stderr, "writing image..."
img.save("rendered.png")

print >>sys.stderr, "dumping terrain data..."

data = []

for j in xrange(img.size[1]):
    for i in xrange(img.size[0]):
        data.append({
            'terrain': REVERSE_COLORS[img_data[i, j]]
        })

print >>sys.stderr, "writing json..."
json.dump(data, sys.stdout)

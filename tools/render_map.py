#!/usr/bin/env python2
#encoding:utf-8
import sys

STRIDE = 150

legend = {
    "ocean": 60,
    "coast": 238,
    "lakeshore": 24,
    "lake": 60,
    "river": 24,
    "marsh": 240,
    "ice": 123,
    "beach": 138,
    "road1": 235,
    "road2": 237,
    "road3": 239,
    "bridge": 241,
    "lava": 167,
    "snow": 15,
    "tundra": 249,
    "bare": 102,
    "scorched": 240,
    "taiga": 108,
    "shrubland": 102,
    "temperate-desert": 186,
    "temperate-rain-forest": 65,
    "temperate-deciduous-forest": 65,
    "grassland": 107,
    "subtropical-desert": 180,
    "tropical-rain-forest": 65,
    "tropical-seasonal-forest": 65
}

class Markers:
    me = "\x1b[48;5;226m" + "\x1b[38;5;0m" + "u!"

def make_escape(c):
    return "\x1b[1m\x1b[48;5;{color}m".format(color=c)

import json
with open("terrain.json") as f:
    map = json.load(f)

x, y = 46, 128

for j in range(STRIDE):
    for i in range(STRIDE):
        if x - 25 <= i < x + 25 and y - 25 <= j < y + 25:
            e = make_escape(legend[map[j * STRIDE + i]["terrain"]])
            if i == x and j == y:
                sys.stdout.write(Markers.me)
            else:
                sys.stdout.write(e + "  ")
    print("\x1b[0m")

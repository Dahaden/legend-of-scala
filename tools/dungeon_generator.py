#!/usr/bin/env python2
#encoding:utf-8

import random
import sys

NORTH = 1
SOUTH = 2
WEST = 4
EAST = 8

MIN_FORKS = 5
MAX_FORKS = 10

MIN_STRAYING = 1
MAX_STRAYING = 5

def generate_dungeon(width, height):
    trunk = []

    n = width * height
    dungeon = [0] * n

    start = (width // 2, height - 1)

    x, y = start
    dungeon[y * width + x] = SOUTH

    sidewayed = [None] * n
    backwarded = [False] * n

    while True:
        trunk.append((x, y))

        opts = [(0, -1)]

        if sidewayed[y] is None:
            opts.extend([(1, 0), (-1, 0)])
        else:
            opts.extend([sidewayed[y]])

        opts = [(dx, dy)
                for (dx, dy) in opts
                if 0 <= x + dx < width]

        dx, dy = random.choice(opts)
        nx, ny = x + dx, y + dy

        if y == 0:
            break

        if (dx, dy) == (0, -1):
            dungeon[ny * width + nx]    |= SOUTH
            dungeon[y * width + x]      |= NORTH

        if (dx, dy) == (1, 0):
            dungeon[ny * width + nx]    |= WEST
            dungeon[y * width + x]      |= EAST
            sidewayed[y] = (1, 0)

        if (dx, dy) == (-1, 0):
            dungeon[ny * width + nx]    |= EAST
            dungeon[y * width + x]      |= WEST
            sidewayed[y] = (-1, 0)

        x = nx
        y = ny

    branchable = [(x, y) for (x, y) in trunk[:-1]
                  if dungeon[y * width + x] & NORTH != 0]

    random.shuffle(branchable)

    for _ in range(random.randint(MIN_FORKS, MAX_FORKS)):
        if not branchable: break

        x, y = branchable.pop()

        for _ in range(random.randint(MIN_STRAYING, MAX_STRAYING)):
            opts = [(dx, dy)
                    for (dx, dy) in [(0, -1), (1, 0), (-1, 0)]
                    if 0 <= x + dx < width and
                       0 <= y + dy < height and
                       dungeon[(y + dy) * width + (x + dx)] == 0]

            if not opts:
                break

            dx, dy = random.choice(opts)
            nx, ny = x + dx, y + dy

            if (dx, dy) == (0, -1):
                dungeon[ny * width + nx]    |= SOUTH
                dungeon[y * width + x]      |= NORTH

            if (dx, dy) == (0, 1):
                dungeon[ny * width + nx]    |= NORTH
                dungeon[y * width + x]      |= SOUTH

            if (dx, dy) == (1, 0):
                dungeon[ny * width + nx]    |= WEST
                dungeon[y * width + x]      |= EAST

            if (dx, dy) == (-1, 0):
                dungeon[ny * width + nx]    |= EAST
                dungeon[y * width + x]      |= WEST

            x = nx
            y = ny

    return dungeon, trunk[-1]


def draw_dungeon(dungeon, end, width, height):
    for y in range(height):
        for x in range(width):
            if (x, y) == (width // 2, height - 1):
                sys.stdout.write("\x1b[38;5;46m")
            elif (x, y) == end:
                sys.stdout.write("\x1b[38;5;196m")
            i = y * width + x
            sys.stdout.write({
                0: " ",
                NORTH: "╵",
                SOUTH: "╷",
                WEST: "╴",
                EAST: "╶",
                NORTH | SOUTH: "│",
                WEST | EAST: "─",
                NORTH | EAST: "└",
                NORTH | WEST: "┘",
                SOUTH | EAST: "┌",
                SOUTH | WEST: "┐",
                NORTH | SOUTH | EAST: "├",
                NORTH | SOUTH | WEST: "┤",
                NORTH | WEST | EAST: "┴",
                SOUTH | WEST | EAST: "┬",
                NORTH | SOUTH | EAST | WEST: "┼"
            }[dungeon[i]])
            sys.stdout.write("\x1b[0m")
        print ""

def expand_dungeon(dungeon, width, height):
    expanded = [" "] * (width * 3 * height * 3)

    width3 = width * 3

    for y in range(height):
        for x in range(width):
            x3 = x * 3
            y3 = y * 3

            v = dungeon[y * width + x]

            if v & NORTH:
                expanded[y3 * width3 + (x3 + 1)] = "#"

            if v & SOUTH:
                expanded[(y3 + 2) * width3 + (x3 + 1)] = "#"

            if (v & NORTH and v & SOUTH and not (v & WEST) and not (v & EAST)) or \
               (v & WEST and v & EAST and not (v & NORTH) and not (v & SOUTH)):
                expanded[(y3 + 1) * width3 + (x3 + 1)] = "#"

            if v & WEST:
                expanded[(y3 + 1) * width3 + x3] = "#"

            if v & EAST:
                expanded[(y3 + 1) * width3 + (x3 + 2)] = "#"

    return expanded

WIDTH = 10
HEIGHT = 10

d, e = generate_dungeon(WIDTH, HEIGHT)
draw_dungeon(d, e, WIDTH, HEIGHT)

exd = expand_dungeon(d, WIDTH, HEIGHT)

for y in range(HEIGHT * 3):
    for x in range(WIDTH * 3):
        sys.stdout.write(exd[y * WIDTH * 3 + x])
    print ""

#!/usr/bin/env python2
#encoding:utf-8

import random
import sys

NORTH = 1
SOUTH = 2
WEST = 4
EAST = 8

def generate_dungeon(width, height):
    trunk = []

    n = width * height
    dungeon = [0] * n

    start = (width / 2, height - 1)
    q = [(width / 2, height - 1)]

    x, y = start
    dungeon[y * width + x] = SOUTH

    sidewayed = [None] * n
    backwarded = [False] * n

    while q:
        x, y = q.pop()
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

        q.append((x + dx, y + dy))

    branchable = [(x, y) for (x, y) in trunk
                  if dungeon[y * width + x] & NORTH != 0]

    random.shuffle(branchable)

    for _ in range(random.randint(5, 10)):
        q = [branchable.pop()]

        for _ in range(random.randint(1, 5)):
            x, y = q.pop()

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

            q.append((nx, ny))

    return dungeon


def draw_dungeon(dungeon, width, height):
    for y in range(height):
        for x in range(width):
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
            }[dungeon[i]]),
        print ""


WIDTH = 50
HEIGHT = 25

draw_dungeon(generate_dungeon(WIDTH, HEIGHT), WIDTH, HEIGHT)

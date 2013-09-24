#!/usr/bin/env python2
#encoding:utf-8

import random
import sys

MIN_FORKS = 5
MAX_FORKS = 10

MIN_STRAYING = 1
MAX_STRAYING = 5

def generate_dungeon(width, height):
    branchable = []

    n = width * height
    dungeon = [False] * n

    x, y = (width // 2, height - 1)
    dungeon[y * width + x] = True

    sidewayed = [None] * height

    while y != 0:
        branchable.append((x, y))

        opts = [(0, -1)]

        if sidewayed[y] is None:
            opts.extend([(1, 0), (-1, 0)])
        else:
            opts.extend([sidewayed[y]])

        opts = [(dx, dy)
                for (dx, dy) in opts
                if 0 <= x + dx < width]

        dx, dy = random.choice(opts)
        if dy == 0:
            sidewayed[y] = (dx, dy)

        x += dx
        y += dy

        dungeon[y * width + x] = True

    ex, ey = x, y

    random.shuffle(branchable)

    for _ in range(random.randint(MIN_FORKS, MAX_FORKS)):
        if not branchable: break

        x, y = branchable.pop()

        for _ in range(random.randint(MIN_STRAYING, MAX_STRAYING)):
            opts = [(dx, dy)
                    for (dx, dy) in [(0, -1), (1, 0), (-1, 0)]
                    if 0 <= x + dx < width and
                       1 <= y + dy < height - 1 and
                       dungeon[(y + dy) * width + (x + dx)] == 0]

            if not opts:
                break

            dx, dy = random.choice(opts)
            x, y = x + dx, y + dy

            dungeon[y * width + x] = True

    return dungeon, (ex, ey)


def draw_dungeon(dungeon, end, width, height):
    for y in range(height):
        for x in range(width):
            if (x, y) == (width // 2, height - 1):
                sys.stdout.write("\x1b[38;5;46m")
            elif (x, y) == end:
                sys.stdout.write("\x1b[38;5;196m")
            i = y * width + x
            if dungeon[i]:
                sys.stdout.write("#")
            else:
                sys.stdout.write(" ")
            sys.stdout.write("\x1b[0m")
        print ""


WIDTH = 50
HEIGHT = 25

d, e = generate_dungeon(WIDTH, HEIGHT)
draw_dungeon(d, e, WIDTH, HEIGHT)

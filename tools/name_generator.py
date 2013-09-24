#!/usr/bin/env python2
import random

PREFIXES = [
    "soul",
    "death",
    "blood",
    "mega",
    "cold",
    "ice",
    "flame",
    "fire",
    "sky",
    "evil",
    "light",
    "radiant",
    "dark",
    "ultra",
    "pwn"
]

SUFFIXES = [
    "angel",
    "demon",
    "thirster",
    "king",
    "killer",
    "killah",
    "destroyer",
    "ripper",
    "reaper",
    "luster",
    "mage",
    "magus",
    "warrior",
    "soldier",
    "vizier",
    "priest",
    "ninja"
]

TRANSFORMS = [
    lambda x: "".join(c.lower() if i % 2 == 0 else c.upper() for i, c in enumerate(x)),
    lambda x: x.upper(),
    lambda x: "".join(c.upper() if c != "i" else c for c in x),
    lambda x: x.replace("o", "0"),
    lambda x: x.replace("i", "1"),
    lambda x: "xX" + x + "Xx"
]

TEMPLATES = [
    lambda x: x + str(random.randint(1000, 99999))
]

print random.choice(TEMPLATES)(random.choice(TRANSFORMS)(random.choice(PREFIXES) + random.choice(SUFFIXES)))

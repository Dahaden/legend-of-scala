import json
import sys

w = int(sys.argv[1])
h = int(sys.argv[2])

with open(sys.argv[3]) as f:
    d = json.load(f)

for j in range(w):
    buf = ""
    for i in range(h):
        if d[j * w + i]["terrain"] == "cave":
            buf += "#"
        else:
            buf += " "
    print(buf)

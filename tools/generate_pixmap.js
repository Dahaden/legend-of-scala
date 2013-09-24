var antsy = require("antsy");
var fs = require("fs");
var PNG = require("png-js");

var png = PNG.load(process.argv[2]);

function pad(x, n, z) {
    var len = n - x.length;
    return new Array(len + 1).join(z) + x;
}

function make_escape(c) {
    return "\x1b[1m\x1b[48;5;" + c + "m";
}

png.decode(function (pixels) {
    var buf = "";
    for (var y = 0; y < png.height; ++y) {
        var line = "";
        for (var x = 0; x < png.width; ++x) {
            var i = (png.width * y + x) << 2;

            var r = pixels[i];
            var g = pixels[i + 1];
            var b = pixels[i + 2];
            var a = pixels[i + 3];

            var c = "#" + pad(r.toString(16), 2, "0") +
                          pad(g.toString(16), 2, "0") +
                          pad(b.toString(16), 2, "0");


            if (a == 0) {
                line += "  ";
            } else {
                line += make_escape(antsy.get_color(c)) + "  " + "\x1b[0m";
            }
        }
        buf += line + "\n";
    }
    console.log(buf);
});

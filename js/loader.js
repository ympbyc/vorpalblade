(function () {
    'use strict';
    var fs = require('fs');
    var http = require('http');
    var url = require('url');

    http.createServer(function (req, res) {

        var req_url = url.parse(req.url);
        console.log(req_url.pathname);
        if (req_url.pathname !== '/') try {
            return res.end(fs.readFileSync(req_url.pathname.slice(1), 'utf8'));
            } catch (err) { return res.end('X|');  }

        var tmpl = rf('index.html');
        var scm = {};
        scm.vopal = rf('scm/vopal.scm');
        scm.mapGen = rf('scm/map-gen.scm');
        scm.creature = rf('scm/creature.scm');
        scm.display = rf('scm/display.scm');
        scm.helper = rf('scm/helper.scm');
        var html = template(tmpl, scm);

        res.writeHead(200, {'Content-Type': 'text/html'});
        return res.end(html);

    }).listen(8080);


    function rf (fname) {
        return fs.readFileSync(fname, 'utf8');
    }
}());


function hashFold (hash, init, fn) {
    var key, last;
    last = init;
    for (key in hash)
        if ({}.hasOwnProperty.call(hash, key))
            last = fn(hash[key], key, last); //destructive binding
    return last;
}

function template (tmpl, filler) {
    return hashFold(filler, tmpl, function (val, key, tmpl) {
        return tmpl.replace(new RegExp("{{"+key+"}}", "g"), val);
    });
}

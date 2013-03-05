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

        var tmpl = fs.readFileSync('index.html', 'utf8');
        var vopalScm = fs.readFileSync('scm/vopal.scm', 'utf8');
        var html = template(tmpl, {
            vopalScm: vopalScm
        });

        res.writeHead(200, {'Content-Type': 'text/html'});
        return res.end(html);

    }).listen(8080);
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

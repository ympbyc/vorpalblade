(function () {
    'use strict';
    var fs = require('fs');
    var http = require('http');
    var url = require('url');

    http.createServer(function (req, res) {

        var req_url = url.parse(req.url);
        console.log(req_url.pathname);
        if (req_url.pathname !== '/') try {
            res.setHeader("Access-Control-Allow-Origin", "*");
            return res.end(rf(req_url.pathname.slice(1), res));
        } catch (err) { return res.end('X|');  }

        var tmpl = rf('index.html', res);
        var scm = {};
        scm.vopal = rf('scm/vopal.scm', res);
        scm.mapGen = rf('scm/map-gen.scm', res);
        scm.creature = rf('scm/creature.scm', res);
        scm.display = rf('scm/display.scm', res);
        scm.helper = rf('scm/helper.scm', res);
        var html = template(tmpl, scm);

        res.writeHead(200, {'Content-Type': 'text/html'});
        return res.end(html);

    }).listen(8080);

    function rf (fname, res) {
        res.setHeader('Content-Type', content_type(fname));
        if (is_blob(fname))
            return fs.readFileSync(fname);
        return fs.readFileSync(fname, 'utf8');
    }
}());

function content_type (fname) {
    var x = fname.split('.');
    var extension = x[x.length-1];

    var c_type = {
        png: 'image/png'
    ,   jpg: 'image/jpeg'
    ,   gif: 'image/gif'
    ,   css: 'text/css'
    ,   js:  'text/javascript'
    ,   html:'text/html'
    ,   txt: 'text/plain'
    }[extension];

    return c_type || 'text/plain';
}

function is_blob (fname) {
    var x = fname.split('.');
    return ['png','jpg','gif'].indexOf(x[x.length-1]) > -1;
}


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

Vorp@lblade
===========

Roguelike game written in BiwaScheme

Run
---

```
cd vorpalblade
node server.js
```

then point your browser to [`http://localhost:8080`](http://localhost:8080)

Status
------

```
ympbyc the programmer                       Chaotic human hacker
The Dungeons of Doom: level 1
HP: 10(10) Level: 1 Xp: 0 Pw: 2(2) AC: 10 $: 0 available for hire
```

The project is about 10% complete.


Project composition
-------------------

+ scm/ contains some code in biwascheme
+ lispy/ contains some lispyscript code that need speed
+ js/ contains compiled lispyscript code
+ lib/ contains js libraries: biwascheme, lispyscript, rot.js, mapper, and CLOS-JS
+ index.html is the main user interface
+ server.js is a server for development use
+ css/ and img/ contains stylesheets and images


License
-------

MIT

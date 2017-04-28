#!/usr/bin/env bash


rm -r dist
mkdir dist

pulp build
purs bundle output/**/*.js -m Example1 -o dist/Main1.js
purs bundle output/**/*.js -m Example2 -o dist/Main2.js
purs bundle output/**/*.js -m Example3 -o dist/Main3.js
purs bundle output/**/*.js -m Example4 -o dist/Main4.js
purs bundle output/**/*.js -m Example5 -o dist/Main5.js
purs bundle output/**/*.js -m Example6 -o dist/Main6.js
purs bundle output/**/*.js -m Example7 -o dist/Main7.js
purs bundle output/**/*.js -m Example8 -o dist/Main8.js
purs bundle output/**/*.js -m Example9 -o dist/Main9.js
purs bundle output/**/*.js -m Example9ST -o dist/Main9ST.js

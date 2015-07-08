#!/usr/bin/env bash


rm -r dist
mkdir dist

pulp build
psc-bundle output/**/*.js -m Example1 -o dist/Main1.js
psc-bundle output/**/*.js -m Example2 -o dist/Main2.js
psc-bundle output/**/*.js -m Example3 -o dist/Main3.js
psc-bundle output/**/*.js -m Example4 -o dist/Main4.js
psc-bundle output/**/*.js -m Example5 -o dist/Main5.js
psc-bundle output/**/*.js -m Example6 -o dist/Main6.js
psc-bundle output/**/*.js -m Example7 -o dist/Main7.js
psc-bundle output/**/*.js -m Example8 -o dist/Main8.js
psc-bundle output/**/*.js -m Example9 -o dist/Main9.js
# psc-bundle output/**/*.js -m Example9ST -o dist/Main9ST.js

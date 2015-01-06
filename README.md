Examples for [purescript-webgl](https://github.com/jutaro/purescript-webgl)

The first n lessons form the page [learningwebgl](http://learningwebgl.com/blog/) ported to
purescript.

Build with:
~~~
npm install
bower update
grunt example1 (2,...)
~~~

Then open index.html in browser. Later examples may need a different index file, 
e.g. index7.html for example7.

A recent purescript compiler from git repo is needed, as we need patch 
"Lift restriction on underscores in unames" from Dec 25, 2014.

For later examples you need to start chrome with --allow-file-access-from-files 
to be able to load local files for textures. I'm not aware how other browsers react.

Have peace.
Jürgen



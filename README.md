# cl-shiritori
Common Lisp-based Shiritori word-chaining game.

This is my first foray into the world of lisp(s), and a project for [CSC 458: Symbolic Programming](http://reed.cs.depaul.edu/peterh/class/csc458/), at DePaul University.

Crude instructions--you need [Hunchentoot](https://edicl.github.io/hunchentoot/) and [cl-who](https://common-lisp.net/~loliveira/ediware/cl-who/doc/).

1. Place .txt files contained in the datasets directory to a default lisp location--for me it's "C:\\". 

2. Load the shiritori.asd file with something like Allegro CL.

3. Load the system: (asdf:load-system 'shiritori)

4. Visit http://localhost:5067/menu in your browser.

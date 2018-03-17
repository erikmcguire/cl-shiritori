# cl-shiritori
Common Lisp-based Shiritori word-chaining game.

Crude instructions--assumes you have Quicklisp, Hunchentoot, and cl-who, and something like Allegro CL.

1. Place .txt files contained in the datasets directory to a default lisp location--for me it's "C:\\". 

2. Edit packages.lisp load paths to point to the correct .lisp file locations.

3. Open Allegro CL, load packages.lisp.

4. Load http://localhost:5067/menu in your browser.

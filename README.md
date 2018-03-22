# cl-shiritori
Common Lisp-based [*shiritori*](https://www.japantimes.co.jp/life/2017/01/16/language/shiritori-simple-game-thats-great-practicing-japanese-vocab/) word-chaining game.

This is my first foray into the world of lisp(s), and a project for [CSC 458: Symbolic Programming](http://reed.cs.depaul.edu/peterh/class/csc458/), at DePaul University. As this was intended, at this stage, as an educational activity, note especially that the functions for converting scripts were custom built from zero, the most challenging element there being the function for converting romaji input (e.g., *inaka*) into kana (e.g., いなか). Future versions will likely use pre-existing conversion tools.

Quick instructions--you need [Hunchentoot](https://edicl.github.io/hunchentoot/) and [cl-who](https://common-lisp.net/~loliveira/ediware/cl-who/doc/).

1. Place .txt files contained in the datasets directory to a default lisp location--for me it's "C:\\". 

2. Load the shiritori.asd file with something like Allegro CL.

3. Load the system with [ASDF](https://common-lisp.net/project/asdf/) or [Quicklisp](https://www.quicklisp.org/beta/): `(asdf:load-system 'shiritori)` or `(ql:quickload :shiritori)`

4. Visit http://localhost:5067/menu in your browser. You should see something like this:

![Image](https://erikmcguire.github.io/assets/img/cls menu ss.png)

This activity is useful for learners of Japanese to practice their productive and receptive vocabulary skills. It is also touted as a [creativity](http://tedtalkspsychology.com/play-this-game-to-come-up-with-original-ideas-with-shimpei-takahashi/) tool, and is used in neuroscientific experiments for a range of purposes (Shimomura, et al., [2008](https://goo.gl/1R8bSW); Kashida, et al., [2016](https://goo.gl/He7Fau); Kato, et al., [2017](https://goo.gl/3sgZZJ)).

This early, simple version of the game is intended primarily for my own education in working with Lisp, although I hope to render it into an [open](http://www.xinhuanet.com/english/2017-06/04/c_136338015.htm), accessible tool for [researchers](https://www.nature.com/news/why-scientists-must-share-their-research-code-1.20504) of various [backgrounds](https://www.nature.com/nature/journal/v541/n7638/full/nj7638-563a.html) and language learners, along with versions containing additional features: more robust import and export functionality, interface localization into Japanese, Chinese and Korean support, parts-of-speech filtering, a measure of gamification (via a progress bar, time limits for what Paul Nation calls '[fluency](https://profesorbaker.wordpress.com/2011/04/02/dr-paul-nation-explains-the-4-3-2-fluency-activity/)' exercises, and scoring \[perhaps with multiple AI players\]), audio, and chains involving the morphemes that kanji represent.

I am partial to offline tools which do not rely on the caprices of website availability, but accessing online dictionaries via API is another feature to consider.

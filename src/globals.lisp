(in-package :shiritori)

(defvar *h* (make-instance 'easy-acceptor :port 5067))
(defparameter *user-opt* nil) ; Allow/forbid kanji word prompts.
(defparameter *pos* nil) ; Allow/forbid -ru endings.
(defparameter *word* nil) ; Word prompt: User supplies response.
(defparameter *rhead* nil) ; For computer chaining.
(defparameter *dict* nil) ; Level-specific hash table.
(defparameter *corr-resp* nil) ; Correct responses.
(defparameter *wrong-resp* nil) ; Wrong response.
(defparameter *missed-words* nil) ; Missed prompts.
(defparameter *export* nil) ; Toggle export missed prompts.
(defparameter *seen* nil) ; Avoid repeated prompts.

(defparameter *n-all* "/jlpt-all.txt")
(defparameter *dict-all* (make-hash-table :test 'equal))
(defparameter *dicth* (make-hash-table :test 'equal)) ; letter(s)->hiragana
(defparameter *dictk* (make-hash-table :test 'equal)) ; letter(s)->katakana
(defparameter *all-kana* nil)
(defparameter *all-kanji* nil)

(defparameter youon '("ゃ" "ゅ" "ょ" "ャ" "ュ" "ョ"))

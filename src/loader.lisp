(in-package :shiritori)

; Mappr macro modified from: http://reed.cs.depaul.edu/peterh/class/csc458/ to take function (e.g., mapcar, maphash) as argument.

(defmacro mappr (var args body colls)
  "Maps lambdas to collections."
  `(funcall ,var (lambda ,args
              ,@body)
            ,colls))

(defun get-words (p d)
  "Obtain list of words from user-specified file."
  (with-open-file (s p :if-does-not-exist nil
                       :external-format :utf-8)
    (loop for l = (read-line s nil)
        while l
        do (if (null (search " " l))
                (setf spsl 0)
                (setf spsl (search " " l)))
        do (setf subl (subseq l 0 spsl))
          when (>= (length (remove #\, subl)) 2)
            do (setf (gethash (remove #\, subl) d)
                     (string-trim '(#\/ #\space) (subseq l (search " " l) (search " -" l))))
          when (>= (length (remove #\, subl)) 2)
            collect (remove #\, subl))))

; For checking legitimate words in corpus.

(defparameter *all-words* (get-words *n-all* *dict-all*))

(mappr 'maphash (k v)
  ((when v (if (equal "" v)
    (setf (gethash k *dict-all*) nil))))
  *dict-all*)

(mappr 'maphash (k v)
  ((setf *all-kanji* (push k *all-kanji*))
    (when v (if (not (equal v ""))
        (setf *all-kana* (push v *all-kana*))))) *dict-all*)

(defun export-missed (p)
  "Export missed words to tab-separated text."
  (with-open-file (s p :direction :output
                       :if-exists :supersede
                       :external-format :utf-8)
    (mapcar (lambda (w)
              (maphash (lambda (k v)
                        (cond ((and v (or (equal w v) (equal w k)))
                                (format s "~a~c~a~%~^" k #\Tab v))
                              ((equal w k)
                                (format s "~a~%~^" k))))
                        *dict*))
            *missed-words*)))

(defun get-kana (p d)
 "Obtain letter->kana Hepburn mappings from user-specified file."
 ; Hepburn mappings modified from:
 ; https://github.com/mhagiwara/nltk/blob/master/jpbook/romkan.py."
 (with-open-file (s p :if-does-not-exist nil
                      :external-format :utf-8)
   (let ((l (read s nil)))
       (mapcar (lambda (x) (setf (gethash (car x) d) (cdr x))) l))))

; Create hash tables for letter-kana correspondences.
(get-kana "/hiragana-grouped.txt" *dicth*)
(get-kana "/katakana-grouped.txt" *dictk*)

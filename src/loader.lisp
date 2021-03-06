(in-package :shiritori)

; Mappr macro modified from: http://reed.cs.depaul.edu/peterh/class/csc458/ to take function (e.g., mapcar, maphash) as argument.

(defmacro mappr (var args body colls)
  "Maps lambdas to collections."
  `(funcall ,var (lambda ,args
              ,@body)
            ,colls))

(defun get-words (p d)
  "Obtain list of words from file."
  (with-open-file (s p :external-format :utf-8)
    (loop for l = (read-line s nil)
        while l
        do (if (null (search " " l))
                (setf spsl 0)
                (setf spsl (search " " l)))
        do (setf subl (subseq l 0 spsl))
          when (>= (length (remove #\, subl)) 2)
            do (setf (gethash (remove #\, subl) d)
                     (string-trim '(#\/ #\space) (subseq l (search " " l) (search " -" l)))))))

; For checking legitimate words in corpus.

(get-words "/jlpt-all.txt" *dict-all*)

(defun user-import (p d &optional (dl "	"))
  "Import delimited file into hash-table and lists.
   Tab-delimited by default."
  (with-open-file (s p :external-format :utf-8)
  (loop for l = (read-line s nil)
    while l
      do (let ((headword
                  (remove-if-not (lambda (x) (alphanumericp x))
                                 (subseq l 0 (search dl l))))
              (yomi
                  (remove-if-not (lambda (x) (alphanumericp x))
                                 (subseq l (search dl l)))))
            (setf (gethash headword d) yomi)
            (setf *kanji*
                  (remove-duplicates (push headword *kanji*) :test 'equal)
                  *kana*
                  (remove-duplicates (push headword *kana*) :test 'equal))
            (setf (gethash headword *dict-all*) yomi))
   )))

(mappr 'maphash (k v)
  ((when v (if (equal "" v)
    (setf (gethash k *dict-all*) nil))))
  *dict-all*)

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

(defun get-kana (d l)
 "Obtain letter->kana Hepburn mappings."
 ; Hepburn mappings modified from:
 ; https://github.com/mhagiwara/nltk/blob/master/jpbook/romkan.py."
 (mapcar (lambda (x) (setf (gethash (car x) d) (cdr x))) l))

; Create hash tables for letter-kana correspondences.
(get-kana *dicth* *hiragana-map*)
(get-kana *dictk* *katakana-map*)

(in-package :shiritori)

(defparameter *n-all* "/jlpt-all.txt")
(defparameter *dict-all* (make-hash-table :test 'equal))
(defparameter *dicth* (make-hash-table :test 'equal)) ; letter(s)->hiragana
(defparameter *dictk* (make-hash-table :test 'equal)) ; letter(s)->katakana
(defparameter *all-kana* nil)
(defparameter *all-kanji* nil)

; Mappr macro from: http://reed.cs.depaul.edu/peterh/class/csc458/ to take function (e.g., mapcar, maphash) as argument.

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

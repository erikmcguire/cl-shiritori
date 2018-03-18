(in-package :shiritori)

(defun call-mappr ()
  "For initial set-up of JLPT Nx corpus."
  (mappr 'maphash (k v)
    ((setf *kanji* (push k *kanji*))
      (when v
        (if (search " " v)
          (setf v (string-trim '(#\/ #\space) (subseq v (search " " v) (search " " v :from-end t)))))
        (if (not (equal (remove #\/ v) ""))
                (setf *kana* (push (remove #\/ v) *kana*))))) *dict*))

; Using 96 as a difference to convert kana: a trick I came up with that seems to work...

(defun kata->hira (word)
  "Convert katakana to hiragana."
  (format nil "~{~a~}"
    (loop for char across word
      when (and (> (char-code char) 12447)
                (< (char-code char) 12544))
      collect (code-char (- (char-code char) 96)))))

(defun hira->kata (word)
  "Convert hiragana to katakana."
  (format nil "~{~a~}"
    (loop for char across word
      when (and (> (char-code char) 12351)
                (< (char-code char) 12448))
      collect (code-char (+ (char-code char) 96)))))

(defun romajip (s)
  "True if 0 non-letters present."
  (and (every #'alphanumericp (coerce s 'list))
       (notany #'null (mapcar (lambda (x) (< x 128))
                           (loop for char across s
                             collect (char-code char))))))

(defun roma->kana (s d)
  "Recursively get kana for longest romaji n-grams."
  (when (or (gethash s d) (> (length s) 1))
    (let ((ls 0)
          (cnt 0))
      (labels ((rk-loop (s d)
                "Collect possible kana values for romaji."
                (car (remove-if #'null
                   (loop for i from 4 downto 1 ; Longest mapping is 4 graphemes.
                       do (setf ss
                            (subseq s 0 (rem i (1+ (length s))))) ; Capture window.
                       when (gethash ss d) ; If there's a match...
                         do (setf ls (length ss)
                                  i (1- i)) ; To recursively slide window.
                       collect (car (gethash ss d))))))
             (rk-aux (s d)
               (incf cnt)
                 (let ((l (length s)))
                   (cond ((or (> cnt (* l 24)) (not (romajip s))
                              (null s)) nil)
                         ((<= l ls) (gethash s d))
                         (t (cons (rk-loop s d)
                                  (rk-aux (subseq s ls l) d)))))))
    (or (gethash s d)
        (rk-aux s d))))))

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

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
      when (or
              (and (> (char-code char) 12535)
                   (< (char-code char) 12544))
              (and (> (char-code char) 12438)
                   (< (char-code char) 12448)))
        collect char
      when (and (> (char-code char) 12448)
                (< (char-code char) 12535))
        collect (code-char (- (char-code char) 96)))))

(defun hira->kata (word)
  "Convert hiragana to katakana."
  (format nil "~{~a~}"
    (loop for char across word
      when (or
              (and (> (char-code char) 12535)
                   (< (char-code char) 12544))
              (and (> (char-code char) 12438)
                   (< (char-code char) 12448)))
        collect char
      when (and (> (char-code char) 12352)
                (< (char-code char) 12439))
        collect (code-char (+ (char-code char) 96)))))

(defun romajip (s)
  "True if 0 non-letters and 1 or more vowels present."
  (and (remove-if #'zerop (mapcar (lambda (x)
                                    (count x s))
                                  (coerce "aiueo-" 'list)))
       (every #'(lambda (x) (or (alphanumericp x) (char= x #\-))) (coerce s 'list))
       (notany #'null (mapcar (lambda (x) (< x 128))
                           (loop for char across s
                             collect (char-code char))))))

(defun roma->kana (s d)
  "Convert romaji to kana."
  (when (or (gethash s d) (> (length s) 1))
    (let ((ls 0)
          (cnt 0))
      (labels ((rk-loop (s d)
                "Collect possible kana values for romaji."
                (car (last (remove-if #'null
                   (loop for i from 4 downto 1 ; Longest mapping is 4 graphemes.
                       do (setf ss
                            (subseq s 0 (rem i
                                            (1+ (length s))))) ; Capture window.
                       when (gethash ss d) ; If there's a match...
                         do (setf ls (length ss)
                                  i (1- i)) ; To recursively slide window.
                       collect (car (gethash ss d)))))))
               (rk-aux (s d)
                 "Recursively get kana for longest romaji n-grams."
                 (incf cnt)
                   (let ((l (length s)))
                     (cond ((or
                              (null s)
                              (> cnt (* l 24))
                              (and (not (equal "n" s)) (not (romajip s))))
                              nil)
                           (t (cons (rk-loop s d)
                                    (rk-aux (subseq s ls l) d))))))
               (cleanr (s d)
                 "Obtain a clean string from roma->kana output."
                 (let ((cleanres
                   (format nil "~{~a~}"
                     (remove-if #'null
                     (or (gethash s d)
                         (rk-aux s d))))))
                 (unless (equal "" cleanres)
                   cleanres))))
      (cleanr s d)))))

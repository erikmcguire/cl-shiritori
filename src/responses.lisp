(in-package :shiritori)

(defun check-time (tm &optional (lm *lm*))
   (>= (* (/ (- (get-internal-real-time) tm)
          internal-time-units-per-second) 1.0) lm))

(defun get-prompt-tail (word)
  "Set prompt tail to appropriate window."
  (setf word (or (gethash word *dict-all*) ; If kanji, then kana.
                 (consol word); If kata, then hira.
                  word) ; Else hira.
        kanat-w (or (youonp word)
                    (longvp word)
                    (format nil "~a"
                                (elt (reverse word) 0)))))

(defun longvp (word)
  "If word ends with dash, adjust tail."
  (if (string= "ー" (elt (reverse word) 0))
    (setf longv (subseq (reverse word) 1 2))
    (setf longv nil))
  longv)

(defun youonp (word)
  "Get longer tail for youon endings."
  (if (find (elt (reverse word) 0) youon :test #'string=)
    (setf youons (subseq word (- (length word) 2)))
    (setf youons nil))
  youons)

(defun consol (s)
  "Convert katakana to hiragana or leave as-is."
  (let ((s (or (coerce (kata->hira s) 'list)
                s)))
  (if (equal (type-of s) 'cons)
    (setf conss (coerce s 'string))
    (setf conss nil))
  conss))

(defun get-word ()
  (when *pos*
    (if (not (equal *user-opt* "n"))
      (setf wdb *kanji*)
      (setf wdb *kana*))
    ; Computer player continues chain or starts fresh.
    (setf *word* (or *rhead*
                     (elt wdb (random (length wdb)))))
   ; Get new word if and until rules not violated.
   (loop while (check-word *word*)
    do (setf *word* (elt wdb (random (length wdb)))))
   (push *word* *seen*)
   (when *tlm*
     (setf thyme (get-internal-real-time)))
   (hunchentoot:redirect "/get-response")))

(defun check-word (word)
  "Check compliance w/ ending rules."
  (setf kanat-w (get-prompt-tail word))
    (or
      (and (< (length *seen*) (length wdb))
           (member word *seen* :test 'equal))
      (equal kanat-w
            (car (gethash "n" *dicth*))); Skip kana 'n' tails.
      (and
        (equal *pos* "n") ; Honor -ru tail prohibition.
        (equal kanat-w
              (car (gethash "ru" *dicth*))))))

(defun checkn (response)
  "Check user compliance with 'n' rule."
  (equal (get-response-tail response)
         (car (gethash "n" *dicth*))))

(defun usedp (response)
  "True if response used before."
  (remove-if #'null
    (mapcar (lambda (x)
              (find response x :test #'equal))
            (append *corr-resp* *wrong-resp*))))

(defun realwordp (response)
  "Check entire multi-script corpus for response."
  (setf *known-word*
        (block nil (maphash #'(lambda (k v)
                      (if (or (equal response k)
                              (equal (hira->kata response) k)
                              (equal response v)
                              (equal (concatenate 'string response "する") v))
                        (return t) nil)) *dict-all*))))

(defun correctp (response word)
 "Prompt tail == response head."
 (equal (get-prompt-tail word)
        (get-response-head response)))

(defun set-correct (response)
  "Saves correct response, searches for matching
   computer response."
   (push
     (list response
          (gethash response *dict-all*))
     *corr-resp*)
  (setf *corr-resp*
        (mapcar (lambda (x)
          (remove-if #'null
            (remove-duplicates x :test 'equal))) *corr-resp*)
        *rhead*
        ; Check dictionary for prompts to chain response tail.
        ; Add discovered word when its head matches, for prompt.
        (block nil
          (remove-if #'null (mapcar #'(lambda (h)
                      (when (and (not (check-word h)) (find (elt h 0) (get-response-tail response)))
                        (return h))) wdb)))))

(defun set-wrong (response)
  "Saves incorrect response."
  (push
    (list response
         (gethash response *dict-all*))
    *wrong-resp*)
  (setf *missed-words* (remove-if #'null (remove-duplicates (push *word* *missed-words*) :test 'equal)))
  (setf *wrong-resp*
    (mapcar (lambda (x)
      (remove-if #'null
        (remove-duplicates x :test 'equal))) *wrong-resp*)))

(defun get-response-tail (response)
  "Set tail for computer to match."
  (setf kanat-kr (or (youonp response)
                     (longvp response)
                     (subseq (reverse response) 0 1))))

(defun get-response-head (response)
  "Set response head to appropriate window."
  (if (and (>= (length response) 2) ; Capture if yōon.
           (find (subseq response 1 2) youon :test #'string=))
    (setf hrsp (subseq response 0 2))
  (setf hrsp (subseq response 0 1))))

(defun check-response (response)
  "Set response to hiragana."
  (if (and *tlm* (check-time thyme))
    (setf exceeded "exceeded")
    (setf exceeded nil))
  (setf response (or exceeded ; Time's up.
                    (gethash response *dict-all*) ; If kanji, then hira.
                    (consol response) ; If kata, then hira.
                    (roma->kana response *dicth*) ; If romaji, set hira.
                     response)) ; Else hira.
  response)

(in-package :shiritori)

(defparameter *hira-resp* nil)
(defparameter *kata-resp* nil)
(defparameter hrsp nil)
(defparameter hhr "")
(defparameter hkr "")
(defparameter thr "")
(defparameter tkr "")
(defparameter resp-ix nil)
(defparameter kresp-ix nil)
(defparameter hresp-ix nil)
(defparameter word-ix nil)
(defparameter wordk-ix nil)
(defparameter endw nil)
(defparameter endr nil)
(defparameter youon '("ゃ" "ゅ" "ょ" "ャ" "ュ" "ョ"))

(defun cleanp (rsp d)
  "Obtain a clean string from letter->kana functions."
  (setf cleanres
      (format nil "~{~a~}"
        (remove-if #'null
          (roma->kana rsp d))))
  (if (not (equal "" cleanres))
    cleanres))

(defun check-word (word)
  "Check compliance with ending rules."
  (if (setf kana-prompt (gethash word *dict-all*)) ; Get kana tail for non-romaji prompt.
    (setf endw (format nil "~a" (elt (reverse kana-prompt) 0)))
    (setf endw (format nil "~a" (elt (reverse word) 0))))
  (if (and kana-prompt (find endw youon :test #'string=))
    (setf endw (subseq kana-prompt (- (length kana-prompt) 2)))
    (if (find endw youon :test #'string=)
      (setf endw (subseq word (- (length word) 2)))))
  (if (and (string= "ー" (elt (reverse word) 0)))
    (setf endw (subseq (reverse word) 1 2)))
  (or
    (or
      (equal endw (car (gethash "n" *dicth*)))
      (equal endw (car (gethash "n" *dictk*)))) ; Skip kana 'n' tails.
    (and (or (equal *pos* "n") (equal *pos* "no")) ; Honor -ru tail prohibition.
      (or
        (equal endw (car (gethash "ru" *dicth*)))
        (equal endw (car (gethash "ru" *dictk*)))))))

(defun check-n (response)
  "Check user compliance with 'n' rule."
  (notevery #'null (mapcar (lambda (x) (equal x endr))
                           (list
                             (car (gethash "n" *dicth*))
                             (car (gethash "n" *dictk*))
                             "n"))))

(defun usedp (response)
  "True if response used before."
  (or (remove-if #'null
        (mapcar (lambda (x)
          (find response x :test #'equal))
        *corr-resp*))
      (remove-if #'null
        (mapcar (lambda (x)
          (find response x :test #'equal))
        *wrong-resp*))))

(defun realwordp (response)
  "Check entire multi-script corpus for response."
  (setf
        *known-word*
        (block nil (maphash #'(lambda (k v)
                      (if (or (equal response k)
                              (equal response v)
                              (equal (concatenate 'string response "する") v))
                        (return t) nil)) *dict-all*)))
    (if (setf *hira-resp* (cleanp response *dicth*)) ; Convert to hiragana.
      (progn
        (setf hhr (format nil "~a" (elt *hira-resp* 0))
              thr (format nil "~a" (elt (reverse *hira-resp*) 0))) ; Get converted response tails.
        (if (and (>= (length *hira-resp*) 2) (find (subseq *hira-resp* 1 2) youon :test #'string=))
          (setf hhr (subseq *hira-resp* 0 2)))))
    (if (setf *kata-resp* (cleanp response *dictk*)) ; Convert to katakana.
      (progn
        (setf hkr (format nil "~a" (elt *kata-resp* 0))
              tkr (format nil "~a" (elt (reverse *kata-resp*) 0)))
        (if (and (>= (length *hira-resp*) 2) (find (subseq *kata-resp* 1 2) youon :test #'string=))
          (setf hkr (subseq *kata-resp* 0 2)))))
    (or (notevery #'null
            (mapcar (lambda (x)
              (member x (append *all-kana* *all-kanji*) :test 'equal)) (remove-if #'null (list response (hira->kata response)
                                (kata->hira response) *hira-resp*
                                *kata-resp* (hira->kata *hira-resp*) (kata->hira *kata-resp*)))))
          *known-word*))

(defun correctp ()
 "Prompt tail == response head."
 (setf matcha
    (mapcar #'(lambda (x)
                (or
                  (and (not (equal "" word-ix)) (equal word-ix x))
                  (and (not (equal "" x)) (equal x endw))
                  (and (not (equal "" wordk-ix)) (equal wordk-ix x))))
             (remove-if #'null
               (list resp-ix hrsp hhr hkr hresp-ix kresp-ix))))
  (not (every #'null matcha)))

(defun set-correct (response)
  "Saves correct response, searches for matching
   computer response."
  (push
    (list response *hira-resp* *kata-resp*) *corr-resp*)
  (setf
        *corr-resp*
        (mapcar (lambda (x)
          (remove-if #'null
            (remove-duplicates x :test 'equal))) *corr-resp*)
        *rhead*
        ; Check dictionary for prompts to chain response tail.
        ; Add discovered word when its head matches, for prompt.
        (block nil
          (maphash #'(lambda (k v)
                      (when
                        (or
                          (and v
                               (not (equal "" v))
                               (or (find (elt v 0) endr)
                                   (find (elt v 0) thr)
                                   (find (elt v 0) tkr)))
                          (and (not (equal "" k))
                               (find (elt k 0) endr)))
                        (return k))) *dict*))))

(defun set-wrong (response)
  "Saves incorrect response."
  (push
    (list response (gethash response *dict-all*) *hira-resp* *kata-resp*) *wrong-resp*)
  (push *word* *missed-words*)
  (setf *missed-words* (remove-if #'null *missed-words*))
  (setf *wrong-resp*
    (mapcar (lambda (x)
      (remove-if #'null
        (remove-duplicates x :test 'equal))) *wrong-resp*)))

(defun check-response (response)
  "Set up useful variables."
  (if (setf kana-tail (gethash response *dict-all*)) ; Get kana tail for non-romaji response.
    (setf endr (format nil "~a" (elt (reverse kana-tail) 0)))
    (setf endr (format nil "~a" (elt (reverse response) 0))))
  (setf *hira-resp* nil ; Reset converted kana responses.
        *kata-resp* nil
        word *word*
        l (length word)
        resp-ix (subseq response 0 1) ; Response 'head'.
        hresp-ix (kata->hira resp-ix)
        kresp-ix (hira->kata resp-ix) ; Katakana version.
        wordk-ix (subseq (gethash word *dict-all*) (- l 1)) ; Kanji-kana prompt tail (?)
        word-ix (subseq word (- l 1))) ; Kanji prompt tail.
  (if (setf kana-head (gethash response *dict-all*)) ; Get kana head for kanji/kana response.
    (setf hrsp (format nil "~a" (elt kana-head 0)))
    (setf hrsp (format nil "~a" (elt response 0))))
  (if (and kana-head (>= (length kana-head) 2)
      (find (subseq kana-head 1 2) youon :test #'string=))
    (setf hrsp (subseq kana-head 0 2))
    (if (and (>= (length response) 2) (find (subseq response 1 2) youon :test #'string=))
      (setf hrsp (subseq response 0 2)))))

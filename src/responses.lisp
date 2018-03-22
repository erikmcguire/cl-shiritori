(in-package :shiritori)

(defun cleanp (rsp d)
  "Obtain a clean string from letter->kana functions."
  (setf cleanres
      (format nil "~{~a~}"
        (remove-if #'null
          (roma->kana rsp d))))
  (if (not (equal "" cleanres))
    cleanres))

(defun get-prompt-tail (word)
  "Set prompt tail to appropriate window."
  (setf word (or (gethash word *dict-all*) ; If kanji, then kana.
                  word)) ; Already kana.
  (setf kanat-w (format nil "~a" (elt (reverse word) 0)))
  (if (find kanat-w youon :test #'string=)
    (setf kanat-w (subseq word (- (length word) 2))))
  (if (and (string= "ー" (elt (reverse word) 0)))
    (setf kanat-w (subseq (reverse word) 1 2)))
  (setf kanat-w
        (or (coerce (kata->hira kanat-w) 'list) ; If kata tail, then hira.
             kanat-w)) ; Already hira.
  (if (equal (type-of kanat-w) 'cons)
    (setf kanat-w (coerce kanat-w 'string)))
  kanat-w)

(defun check-word (word)
  "Check compliance w/ ending rules."
  (setf kanat-w (get-prompt-tail word))
    (or
      (equal kanat-w
            (car (gethash "n" *dicth*))); Skip kana 'n' tails.
      (and
        (equal *pos* "n") ; Honor -ru tail prohibition.
        (equal kanat-w
              (car (gethash "ru" *dicth*))))))

(defun check-n (response)
  "Check user compliance with 'n' rule."
  (equal (get-response-tail response)
         (car (gethash "n" *dicth*))))

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
  (setf *known-word*
        (block nil (maphash #'(lambda (k v)
                      (if (or (equal response k)
                              (equal response v)
                              (equal (concatenate 'string response "する") v))
                        (return t) nil)) *dict-all*)))
    (or (notevery #'null
            (mapcar (lambda (x)
              (member x (append *all-kana* *all-kanji*) :test 'equal)) (remove-if #'null (list response (hira->kata response)))))
          *known-word*))

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
                      (when (find (elt h 0) (get-response-tail response))
                        (return h))) wdb)))))

(defun set-wrong (response)
  "Saves incorrect response."
  (push
    (list response
         (gethash response *dict-all*))
    *wrong-resp*)
  (push *word* *missed-words*)
  (setf *missed-words* (remove-if #'null (remove-duplicates *missed-words* :test 'equal)))
  (setf *wrong-resp*
    (mapcar (lambda (x)
      (remove-if #'null
        (remove-duplicates x :test 'equal))) *wrong-resp*)))

(defun get-response-tail (response)
  "Set tail for computer to match."
  (setf kanat-kr (subseq (reverse response) 0 1))
  (if (find kanat-kr youon :test #'string=) ; To match yōon.
    (setf kanat-kr (subseq response (- (length response) 2))))
  (if (string= "ー" (elt (reverse response) 0)) ; To match kana before dash.
    (setf kanat-kr (subseq (reverse response) 1 2)))
  kanat-kr)

(defun get-response-head (response)
  "Set response head to appropriate window."
  (if (and (>= (length response) 2) ; Capture if yōon.
           (find (subseq response 1 2) youon :test #'string=))
    (setf hrsp (subseq response 0 2))
  (setf hrsp (subseq response 0 1))))

(defun check-response (response)
  "Set response to hiragana."
  (setf response (or (coerce (kata->hira response) 'list) ; If kata, set hira.
                     (cleanp response *dicth*) ; If romaji, set hira.
                     (gethash response *dict-all*) ; If kanji, set hira.
                      response)) ; Already hira.
  (if (equal (type-of response) 'cons)
    (setf response (coerce response 'string)))
  response)

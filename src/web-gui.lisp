(in-package :shiritori)

(define-easy-handler (get-response :uri "/get-response") (user-in response)
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :style "background-color: aliceblue;"
     (:head (:title "shiritori") (:meta :charset "utf-8"))
      (:body (:div :style "margin: 0 auto; width: 400px;"
        (:p :lang "ja" (str *word*) " →<br>")
        (:form :method :post
         (:input :id "respo" :autofocus "autofocus" :onfocus "this.select();" :type :text :name "response" :value response)
         (:input :type :submit :value "answer") (:br) (:br)
         (:fieldset (:legend "options")
           (:input :type :radio :name "user-in" :value "show" :onchange "this.form.submit();" "show <i>yomi</i>") (:br)
           (:input :type :radio :name "user-in" :value "skip" :onchange "this.form.submit();" "next word") (:br)
           (:input :type :radio :name "user-in" :value "q" :onchange "this.form.submit();" "quit to menu")))
         (when (setf response (or user-in response))
           (setf answer (check-response response))
           (cond ((equal response "skip") ; Prompt skips to next word.
                   (setf *rhead* nil)
                   (hunchentoot:redirect "/get-word"))
                 ((and (equal response "show") ; Prompt w/ yomi if available.
                       (gethash *word* *dict-all*))
                    (htm (:p :lang "ja" (format t "~%<br>~a~%" (gethash word *dict-all*)))))
                 ((and (equal response "show")
                        (not (gethash *word* *dict-all*)))
                     (htm (:p  "<br>It's already <i>kana</i>!<br>")))
                 ((equal "q" response)
                     (format t "~%<br>Quitting to menu.~%" nil)
                     (unless (equal *export* "n")
                      (export-missed (format nil "~a_~a.txt" "output"
                                  (get-universal-time))))
                     (hunchentoot:redirect "/menu"))
                 ((check-n answer) ; End of Line
                     (htm (:p  (format t "~%<br>You lose! You used a word that ends with \"~a\".
                      No words begin with \"~:*~a\", which makes a response impossible.~%" (car (gethash "n" *dicth*)))))
                     (htm (:a :id "ok" :href (format nil "/menu" nil) " <br>OK"))
                     (unless (equal *export* "n")
                       (export-missed (format nil "~a_~a.txt" "output"
                                   (get-universal-time)))))
                  ((usedp answer)
                    (format t "~%<br>You've already used that word.~%" nil))
                  ((not (realwordp answer))
                    (format t "~%<br>Word not in database, unable to verify.~%" nil))
                 ((correctp answer *word*)
                    (format t "~%<br>Correct!~%" nil)
                    (set-correct answer)
                    (htm (:a :id "correct" :href (format nil "/get-word" nil) " <br>>>")))
                 ((not (correctp answer *word*))
                    (format t "~%<br>Sorry, try again.~%" nil)
                    (set-wrong answer)))))))))

(define-easy-handler (get-word :uri "/get-word") ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title "shiritori") (:meta :charset "utf-8"))
      (:body
         (when *pos*
           (if *rhead* ; Computer player continues chain.
             (setf *word* *rhead*)
             (setf *word* nil))
           (if (not (equal *user-opt* "n"))
             (setf wdb *kanji*)
             (setf wdb *kana*))
           (if (or (member *word* *seen*) (null *word*)) ; Pick random word to start fresh.
             (setf *word* (elt wdb (random (length wdb)))))
          (loop while (check-word *word*)
            do (setf *word* (elt wdb (random (length wdb)))))
              ; Get new word if and until rules not violated.
           (if (not (member *word* *seen*))
             (push *word* *seen*))
           (hunchentoot:redirect "/get-response"))))))

(define-easy-handler (menu :uri "/menu") (pos level exmissed akanji)
  (setf (hunchentoot:content-type*) "text/html; charset='utf-8'")
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :style "background-color: aliceblue;"
     (:head (:title "shiritori") (:meta :charset "utf-8"))
      (:body (:h2 :lang "ja" :align :center "尻取り (しりとり)")
        (:p :lang "ja" :style "margin: 0 auto; width: 40%; padding: 5px; border: 3px groove; font-family: georgia, sans-serif; font-size: 12;"
        "
           Welcome to a prototype Common Lisp implementation of the Japanese word-chaining game, <a href=\"https://www.japantimes.co.jp/life/2017/01/16/language/shiritori-simple-game-thats-great-practicing-japanese-vocab/\"><i>shiritori</i></a>.

             <br><br>When prompted with a word, respond with a word whose first syllable matches the final syllable of the prompt. For instance, if prompted with む<b>し</b>, respond with something like <b>し</b>る.

             <br><br>If the final grapheme of the reading is a <a href=\"https://en.wikipedia.org/wiki/Yōon\">small kana</a>, such as with 反射, or its kana form はん<b>しゃ</b>, match the final two kana in your response (e.g., <b>しゃ</b>ちょう).

             <br><br>If the final grapheme is 'ー', such as with エレベータ<b>ー</b>, then match the kana before it (e.g., <b>タ</b>バコ).

             <br><br>The system can recognize <i>hiragana, katakana, kanji,</i> and <i>romaji</i> input.

             <br><br>If enabled, after quitting to menu, you can find exported file(s) in your default lisp folder (e.g., 'C\:\\acl10.1express\\'), named output_[universal_time].txt.

             <br><br>Vocabulary data, for now, is from Wiktionary's <a href=\"https://en.wiktionary.org/wiki/Appendix:JLPT\">lists</a>.")
       (:br)
       (:div :style "margin: 0 auto; width: 20%;"
       (:form :method :post
         (:fieldset (:legend "options")
         (:select :autofocus "autofocus" :id "level" :name "level"
           (loop for ulvl in '("N5" "N4" "N3" "N2" "N1")
             do (htm (:option :value level (str ulvl)))))
         (:label :for "level" " JLPT level") (:br) (:br)
         (:input :type :checkbox :id "exp" :name "exmissed" :value exmissed)
         (:label :for "exp" "export missed") (:br)
         (:input :type :checkbox :id "kmode" :name "akanji" :value akanji)
         (:label :for "kmode" "allow <i>kanji</i> prompts") (:br)
         (:input :type :checkbox :id "ru" :name "pos" :value pos)
         (:label :for "ru" "allow <i>-ru</i> endings")) (:br)
         (:div :style "text-align: center;" (:input :type :submit :value "start the game"))))
        (when level
          (setf *pos* (or pos "n")
                *user-opt* (or (str akanji) "n")
                *export* (or (str exmissed) "n")
                user-level (string-downcase (str level))
                *kana* nil
                *kanji* nil
                *dict* (make-hash-table :test 'equal))
          (get-words (concatenate 'string "/jlpt" user-level ".txt") *dict*)
          (call-mappr)
          ; Starting over, reset lists...
          (setf *corr-resp* nil
                *wrong-resp* nil
                *missed-words* nil)
          (hunchentoot:redirect "/get-word"))))))

(hunchentoot:start *h*)

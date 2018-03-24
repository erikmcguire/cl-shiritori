(in-package :shiritori)

(defmacro feedback (s)
  `(htm (:script
         :type "text/javascript"
         "document.getElementById(\"feedback\").innerHTML ='" ,s "<br><br>';")))

(define-easy-handler (get-response :uri "/get-response") (user-in response)
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :style "background-color: aliceblue;"
     (:head (:title "shiritori") (:meta :charset "utf-8")) (:h1 "<br>")
      (:body (:div :style "margin: 0 auto; width: 400px;"
        (:form :method :post :align "center"
          (:label :lang "ja" :for "respo" (str *word*) " →&nbsp; ")
          (:input :id "respo"
           :autofocus "autofocus"
           :onfocus "this.select();"
           :type :text
           :name "response" :value response)
          (:input :type :submit :value "answer") (:br) (:br)
          (:div :id "feedback" :style "margin: 0 auto; width: 400px;")
          (:fieldset :style "font-family: georgia, sans-serif; font-size: 12;" (:legend "options")
            (:input :type :submit :name "user-in" :value "show" :onchange "this.form.submit();")
            (:input :type :submit :name "user-in" :value "skip" :onchange "this.form.submit();")
            (:input :type :submit :name "user-in" :value "quit" :onchange "this.form.submit();")))
         (when (setf response (or user-in response))
           (setf answer (check-response response))
           (cond ((equal response "skip") ; Prompt skips to next word.
                    (setf *rhead* nil)
                    (hunchentoot:redirect "/get-word"))
                 ((and (equal response "show") ; Prompt w/ yomi if available.
                       (gethash *word* *dict-all*))
                       (feedback (concatenate 'string "<p lang=ja>" (str (gethash *word* *dict-all*)) "</p><br>';")))
                 ((and (equal response "show")
                       (not (gethash *word* *dict-all*)))
                       (feedback "It\\'s already <i>kana</i>!"))
                 ((equal "quit" response)
                     (feedback "Quitting to menu.")
                     (unless (equal *export* "n")
                      (export-missed (funcall *expath*)))
                     (hunchentoot:redirect "/menu"))
                 ((equal "exceeded" answer)
                   (feedback "Sorry, you ran out of time!<br><br><a href=/menu>OK</a>"))
                 ((checkn answer) ; End of Line
                     (feedback "You lose! You used a word that ends with ん. No word begins with ん, which makes a response impossible.<br><br><a href=/menu>OK</a>")
                     (unless (equal *export* "n")
                       (export-missed
                           (funcall *expath*))))
                  ((usedp answer)
                    (feedback "You\\'ve already used that word."))
                  ((not (realwordp answer))
                    (feedback "Word not in database, unable to verify."))
                 ((correctp answer *word*)
                    (set-correct answer)
                    (feedback "Correct!<br><br><a href=/get-word>>></a>"))
                 (t (feedback "Sorry, try again.")
                    (set-wrong answer)))))))))

(define-easy-handler (get-word :uri "/get-word") ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title "shiritori") (:meta :charset "utf-8"))
      (:body
         (get-word)))))

(define-easy-handler (menu :uri "/menu") (pos level exmissed akanji custom delimiter tlm lm expath)
  (setf (hunchentoot:content-type*) "text/html; charset='utf-8'")
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :style "background-color: aliceblue;"
     (:head (:title "shiritori") (:meta :charset "utf-8"))
      (:body (:h2 :lang "ja" :align :center "尻取り (しりとり)")
        (:p :lang "ja" :style "margin: 0 auto; width: 40%; padding: 5px; border: 3px groove; font-family: georgia, sans-serif; font-size: 12;"
         "Welcome to a prototype Common Lisp implementation of the Japanese word-chaining game,
         <a href=https://www.japantimes.co.jp/life/2017/01/16/language/shiritori-simple-game-thats-great-practicing-japanese-vocab/>
         <i>shiritori</i></a>.
         <br><br>When prompted with a word, respond with a word whose first syllable matches the final syllable of the prompt.
         For instance, if prompted with む<b>し</b>, respond with something like <b>し</b>る.
         <br><br>If the final grapheme of the reading is a <a href=https://en.wikipedia.org/wiki/Yōon>small kana</a>,
         such as with 反射, or its kana form はん<b>しゃ</b>, match the final two kana in your response (e.g., <b>しゃ</b>ちょう).
         <br><br>If the final grapheme is 'ー', such as with エレベータ<b>ー</b>, then match the kana before it (e.g., <b>タ</b>バコ).
         <br><br>The system can recognize <i>hiragana, katakana, kanji,</i> and <i>romaji</i> input.
         <br><br>You may set a time limit for each prompt-response pair, between 1-20 seconds (5s by default).
         <br><br>If enabled, after quitting to menu, you can find exported file(s) in your default lisp folder
         (e.g., 'C\:\\acl10.1express\\'), named output_[universal_time].txt, or in an existing directory path of your choosing.
         <br><br>Word data are from public Japanese Language Proficiency Test
          <a href=https://en.wiktionary.org/wiki/Appendix:JLPT>lists</a>.
         <br><br>You may enter the path to a custom import file, also: by default, the .txt or .csv should be tab-delimited
          with two columns: kanji word form and kana form; you can also use comma-delimited (e.g., on a given line: 漢字,かんじ).")
       (:br)
       (:div :style "margin: 0 auto; width: 30%; font-family: georgia, sans-serif; font-size: 12;"
       (:form :method :post
         (:fieldset (:legend "options")
         (:select :autofocus "autofocus" :id "level" :name "level"
           (loop for ulvl in '("N5" "N4" "N3" "N2" "N1")
             do (htm (:option :value level (str ulvl)))))
         (:label :for "level" " JLPT level") (:br) (:br)
         (:input :type :checkbox :id "kmode" :name "akanji" :value akanji)
         (:label :for "kmode" "allow <i>kanji</i> prompts") (:br)
         (:input :type :checkbox :id "ru" :name "pos" :value pos)
         (:label :for "ru" "allow <i>-ru</i> endings") (:br) (:br)
         (:input :type :checkbox :id "tlm" :name "tlm" :value tlm)
         (:label :for "tlm" "limit time")
         (:input :type :number :min "1.0" :max "20.0" :step "1.0" :style "width: 2.5em;" :value lm :name "lm" :placeholder "5s") (:br) (:br)
         (:input :type :checkbox :id "exp" :name "exmissed" :value exmissed)
         (:label :for "exp" "export missed") (:br)
         (:input :type :checkbox :id "delim" :name "delimiter" :value delimiter)
         (:label :for "delim" "comma-delimited import") (:br) (:br)
         (:label :for "imp" "Import path: ")
         (:input :type :text :id "imp" :name "custom" :value custom :placeholder "C\:\\import.txt") (:br)
         (:label :for "expp" "Export path: ")
         (:input :type :text :id "expp" :name "expath" :value expath :placeholder "C\:\\export_lists\\") (:br)) (:br)
         (:div :style "text-align: center;" (:input :type :submit :value "start the game"))))
        (when level
          (when (equal "" lm)
            (setf lm "5.0"))
          (setf *pos* (or pos "n")
                *user-opt* (or (str akanji) "n")
                *export* (or (str exmissed) "n")
                *tlm* tlm
                *lm* (read-from-string lm)
                *kana* nil
                *kanji* nil
                *dict* (make-hash-table :test 'equal))
          (funcall *expath* (str expath))
          (cond ((and delimiter (> (length custom) 0))
                  (user-import (str custom) *dict* ","))
                ((> (length custom) 0)
                  (user-import (str custom) *dict*))
                (t
                  (get-words (concatenate 'string "/jlpt" (string-downcase (str level)) ".txt") *dict*)
                  (call-mappr)))
          ; Starting over, reset lists...
          (setf *corr-resp* nil
                *wrong-resp* nil
                *missed-words* nil)
          (hunchentoot:redirect "/get-word"))))))

(hunchentoot:start *h*)

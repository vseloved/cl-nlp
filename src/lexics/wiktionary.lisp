;;; (c) 2015-2017 Vsevolod Dyomkin

(in-package #:nlp.lexics)
(named-readtables:in-readtable rutilsx-readtable)


(defclass wikt-lemmatizer (lemmatizer)
  ((dict :initarg :dict :accessor lem-dict)))

(defmethod lemmatize ((lemmatizer wikt-lemmatizer) word &optional tag)
  (when-it (? @lemmatizer.dict tag word)
    (values it
            tag)))

(defun extract-wikt-lemma-dict (path)
  (cxml:parse path (make 'wikt-sax)))

(def-lang-var wikt-lemmatizer
    (load-mem-dict (lang-file :en "wikt-dict.txt"))
  "Lemmatizer based on Wiktionary data.")


;;; SAX processing

(defclass wikt-sax (ncorp::sax-progress-mixin)
  ((lang :initarg :lang :initform :en)
   (fn :initarg :fn)
   (texts :initform nil)
   (titles :initform nil)
   (cur-tag :initform nil)
   (cur-text :initform nil)
   (word :initform nil)
   (skip :initform nil)
   (pos-tags :initform #h(equal))
   (word-forms :initform #h(equal))))

(defmethod sax:end-document ((sax wikt-sax))
  sax)

(defmethod sax:start-element ((sax wikt-sax)
                              namespace-uri local-name qname attributes)
  (:= @sax.skip nil
      @sax.cur-text nil
      @sax.cur-tag (mkeyw local-name)))
  
(defmethod sax:characters ((sax wikt-sax) data)
  (case @sax.cur-tag
    (:title (let ((title (string-trim +white-chars+ data)))
              (unless (blankp title)
                (:= @sax.word title))))
    (:text
     (if (or (in# @sax.word @sax.pos-tags)
             (starts-with "#redirect" data :test 'string-equal)
             (find #\Space @sax.word)
             (find #\_ @sax.word))
         (:= @sax.skip t)
         (push data @sax.cur-text)))))

(defparameter *tag-names*
    #h(equal
       "Noun" "NN"
       "Adjective" "JJ"
       "Adverb" "RB"
       "Verb" "VB"))

(defpar *dict* #h(equal))

(defmethod sax:end-element ((sax wikt-sax) namespace-uri local-name qname)
  (with-slots (cur-tag cur-text fn skip) sax
    (when (eql :text cur-tag)
      (unless skip
        (with ((text (strjoin #\Space (reverse cur-text)))
               (by-lang (re:split "==\\w+==[^=]" text))
               (langs (mapcar ^(lang-iso (substr % 2 -3))
                              (re:all-matches-as-strings "==(\\w+)==[^=]"
                                                         text))))
          (when (or (null (length langs))
                    (member @sax.lang langs))
            (when (> (length by-lang) (length langs))
              (push nil langs))
;            (print @sax.word)
            (let ((cur-text (? by-lang (or (position @sax.lang langs)
                                          0))))
              (re:do-matches (beg end "===\\w+===[^=]" cur-text)
                (with ((section (slice cur-text (+ beg 3) (- end 4)))
                       (base-tag (? *tag-names* section)))
                  (when base-tag
                    (with ((beg (1+ end))
                           (end (re:scan "[^=]====\\w" cur-text :start beg)))
                      (if-it (search " of|" cur-text
                                     :test 'string= :start2 beg :end2 end)
                             (loop :for (qual letter)
                                   :in '(("plural" #\S)
                                         ("third-person singular" #\Z)
                                         ("present participle" #\G)
                                         ("past participle" #\N)
                                         ("simple past" #\D)
                                         ("past" #\D)
                                         ("comparative" #\R)
                                         ("superlative" #\S)) :do
                               (when-it (search qual cur-text :test 'string=
                                                :start2 beg :end2 end)
                                 (let ((pos (mkeyw (strcat base-tag letter))))
                                   (push pos (? @sax.pos-tags @sax.word))
                                   (:= (? @sax.word-forms
                                          (list (slice cur-text
                                                       (+ it 4)
                                                       (position #\} cur-text
                                                                 :start (+ it 4)))
                                                base-tag
                                                pos))
                                       @sax.word))
                                 (return)))
                             (push (mkeyw base-tag)
                                   (? @sax.pos-tags @sax.word))))))))))))))

        ;; (let* ((text (strjoin #\Space (reverse cur-text)))
        ;;        (beg (+ 10 (or (re:scan "==English==" text) -10)))
        ;;        (end (re:scan "\\s==\\w" text :start beg))
        ;;        (text (slice text beg end))
        ;;        rez)
        ;;   (loop :for (qualifier pos) :in '(("Noun" tags:NN)
        ;;                                    ("Verb" tags:VB)
        ;;                                    ("Adjective" tags:JJ)
        ;;                                    ("Adverb" tags:RB)) :do
        ;;      (when (search (strcat "===" qualifier "===") text)
        ;;        (push pos rez)))
        ;;   (loop :for (qualifier pos) :in '(("plural" tags:NNS)
        ;;                                    ("third-person singular" tags:VBZ)
        ;;                                    ("past" tags:VBD)
        ;;                                    ("present participle" tags:VBG)
        ;;                                    ("simple past" tags:VBD)
        ;;                                    ("past participle" tags:VBN)
        ;;                                    ("comparative" R)
        ;;                                    ("superlative" S)) :do
        ;;      (let ((str (strcat qualifier " of|")))
        ;;        (when-it (search str text)
        ;;          (ecase pos
        ;;            (tags:NNS (removef rez 'tags:NN))
        ;;            ((tags:VBZ tags:VBD tags:VBN tags:VBG) (removef rez 'tags:VB))
        ;;            ((R S) (cond ((find tags:JJ rez)
        ;;                          (removef rez 'tags:JJ)
        ;;                          (:= pos (ecase pos
        ;;                                    (R 'tags:JJR)
        ;;                                    (S 'tags:JJS))))
        ;;                         ((find tags:RB rez)
        ;;                          (removef rez 'tags:RB)
        ;;                          (:= pos (ecase pos
        ;;                                    (R 'tags:RBR)
        ;;                                    (S 'tags:RBS)))))))
        ;;          (push (pair pos (slice text (+ it (length str))
        ;;                                 (position-if-not #`(or (alpha-char-p %)
        ;;                                                        (char= #\- %))
        ;;                                                  text
        ;;                                                  :start (+ it (length str)))))
        ;;                rez))))
        ;;   (dolist (pos rez)
        ;;     (if (listp pos)
        ;;         (with-pair (tag base) pos
        ;;           (:= (? word-forms base tag) cur-title))
        ;;         (:= (? pos-tags cur-title) pos)))

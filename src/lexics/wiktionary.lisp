;;; (c) 2015 Vsevolod Dyomkin

(in-package #:nlp.lexics)
(named-readtables:in-readtable rutils-readtable)


(defclass lemmatizer () ())

(defclass wikt-lemmatizer (lemmatizer)
  ((dict :initarg :dict :accessor lemma-dict)))

(defmethod lemmatize ((lemmatizer wikt-lemmatizer) word &optional tag)
  (with-slots (dict) lemmatizer
    (or (? dict tag word)
        (? dict nil word)
        word)))

(defun extract-wikt-lemma-dict (path)
  (cxml:parse path (make 'wikt-sax)))

(defclass wikt-sax (sax-with-progress)
  ((fn :initarg :fn)
   (texts :initform nil)
   (titles :initform nil)
   (cur-tag :initform nil)
   (cur-text :initform nil)
   (cur-title :initform nil)
   (skip :initform nil)
   (pos-tags :initform #h(equal))
   (word-forms :initform #h(equal))))

(defmethod sax:start-element ((sax wikt-sax)
                              namespace-uri local-name qname attributes)
  (with-slots (cur-tag skip) sax
    (:= skip nil
        cur-tag (mkeyw local-name))))

(defmethod sax:characters ((sax wikt-sax) data)
  (with-slots (cur-tag cur-title cur-text skip) sax
    (case cur-tag
      (:title (push data cur-title))
      (:text
       (if (starts-with "#redirect" data :test 'string-equal)
           (:= skip t)
           (push data cur-text))))))

(defmethod sax:end-element ((sax wikt-sax) namespace-uri local-name qname)
  (with-slots (cur-tag cur-title cur-text fn skip pos-tags word-forms) sax
    (when (eql :text cur-tag)
      (unless skip
        (when-it (search "form of|" cur-text :test 'string=)
          (print (slice cur-text it 100)))))))

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

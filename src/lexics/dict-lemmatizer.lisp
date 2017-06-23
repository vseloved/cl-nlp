;;; (c) 2016-2017 Vsevolod Dyomkin

(in-package #:nlp.lexics)
(named-readtables:in-readtable rutilsx-readtable)


(defclass mem-dict (dict lemmatizer)
  ((words :initarg :words :initform #h(equal)
                   :accessor dict-words)
   (forms :initarg :forms :initform #h(equal)
                   :accessor dict-forms)
   (pos-precedence :initarg :pos-precedence :initform nil
                   :accessor dict-pos-precedence))
  (:documentation
   "A dictionary-based lemmatizer that keeps all WORDS
    and their FORMS relations in memory."))


(defmethod lookup ((dict mem-dict) word)
  (in# word @dict.words))

(defmethod pos-tags ((dict mem-dict) word)
  (? @dict.words word))

(defmethod lemmatize ((lemmatizer mem-dict) word &optional pos)
  ;; unkown word
  (unless (lookup @lemmatizer.words word)
    (return-from lemmatize))
  (let ((poss (pos-tags lemmatizer word) :test 'equalp))
    (if-it (or (member pos poss)  ; the word is a known form for the requested POS
               (and (null pos)
                    ;; the word is in basic form and no specific POS is requested
                    (member-if ^(= 2 (length (princ-to-string %)))
                               poss)))
           (values word
                   it)
           (with ((word-pos present?
                            (if pos
                                (cond-it
                                  ((? @lemmatizer.forms (word/pos word pos))
                                   (values it
                                           t))
                                  ((? @lemmatizer.forms
                                      (word/pos word (first (mklist pos))))
                                   (values (argmax 'identity it
                                                   :key ^(precedence lemmatizer
                                                                     (? % 0 0)))
                                           t)))
                                (get# word @lemmatizer.forms))))
             (:= word-pos (remove-duplicates word-pos :test 'equalp))
             (if present?
                 ;; there is a known word form for the requested POS
                 (values (? word-pos 0 0)
                         (? word-pos 0 1)
                         (rest word-pos))
                 ;; the word is known but there're no word forms for the requested POS
                 (values nil
                         nil
                         (mapcar ^(pair word %)
                                 poss)))))))

(defmethod lemmatize1 ((lemmatizer mem-dict) word &optional pos)
  (if (derived-form? lemmatizer word)
      (with (((&rest word-pos) (multiple-value-list
                                (lemmatize lemmatizer word pos))))
        (etypecase (first word-pos)
          (null word)
          (string (first word-pos))
          (list (flet ((pos-len (word-pos)
                         (length (princ-to-string (? word-pos 1 0)))))
                  (? (sort word-pos
                           ^(let ((%len (pos-len %))
                                  (%%len (pos-len %%)))
                              (if (= %len %%len)
                                  (< (length (? % 0))
                                     (length (? %% 0)))
                                  (< %len %%len))))
                     0 0)))))
      word))



;; TODO: make generic & language-dependent
(defun derived-form? (lemmatizer word)
  (intersection (mapcar 'atomize (nlp:pos-tags lemmatizer word))
                '(tag:NNS tag:VBD tag:VBG tag:VBN tag:VBZ tag:VBP
                  tag:JJR tag:JJS tag:RBR tag:RBS)))
  
(defun load-mem-dict (in)
  "Load new mem-dict from IN."
  (format *debug-io* "~&Reading mem-dict from ~A:"
          (if (streamp in)
              @in.stream.underfile.pathname
              in))
  (with ((dict (make 'mem-dict))
         ((words forms) @ dict)
         (cur nil)
         (count 0)
         (stream (if (streamp in)
                     in
                     (open in))))
    (unwind-protect
         (loop :for line := (read-line stream nil) :while line :do
           (when (zerop (rem (:+ count) 10000)) (format *debug-io* "."))
           (with (((word tag) (split #\Space line :remove-empty-subseqs t))
                  (tags (mapcar ^(mksym % :package :tag)
                                (split #\: tag))))
             (pushnew tags (? words word) :test 'equalp)
             (if (char= #\Space (? line 0))
                 (:= (? forms word) (if-it (? forms word)
                                           (cons cur it)
                                           (list cur))
                     (? forms (word/pos word (rt cur))) cur
                     (? forms (word/pos word (? (rt cur) 0))) cur
                     (? forms (word/pos (lt cur) tags)) (pair word tags))
                 (:= cur (pair word tags)))))
      (unless (streamp in)
        (close stream)))
    (format *debug-io* " done. (Read ~A words).~%" count)
    dict))


(def-lang-var dict-lemmatizer nil
  "Lemmatizer based on arbitrary dictionary in CL-NLP format.")


;;; util

(defun precedence (lemmatizer pos)
  "POS tag precedence in LEMMATIZER precedence list"
  (or (position pos @lemmatizer.pos-precedence)
      most-positive-fixnum))

;;; (c) 2016 Vsevolod Dyomkin

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
    (return-from lemmatize word))
  (let ((tags (pos-tags lemmatizer word) :test 'equalp))
    (if (member pos tags)
        ;; the word itseld is a known word form for the requested tag
        (values word
                pos)
        (with ((word-tag present?
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
          (if present?
              ;; there is a known word form for the requested tag
              (apply 'values word-tag)
              ;; the word id known but ther's no word forms for the requested ta
              (values word
                      (first tags)))))))

(defun load-mem-dict (file)
  "Load new mem-dict from FILE."
  (format *debug-io* "~&Reading mem-dict from file ~A:" file)
  (with ((dict (make 'mem-dict))
         ((words forms) @ dict)
         (cur nil)
         (count 0))
    (dolines (line file)
      (when (zerop (rem (:+ count) 10000)) (format *debug-io* "."))
      (with (((word tag) (split #\Space line :remove-empty-subseqs t))
             (tags (mapcar ^(mksym % :package :tag)
                           (split #\: tag))))
        (push tags (? words word))
        (if (char= #\Space (? line 0))
            (:= (? forms word) (if-it (? forms word)
                                      (cons cur it)
                                      cur)
                (? forms (word/pos word (rt cur))) cur
                (? forms (word/pos word (? (rt cur) 0))) cur
                (? forms (word/pos (lt cur) tags)) (pair word tags))
            (:= cur (pair word tags)))))
    (format *debug-io* " done. (Read ~A words).~%" count)
    dict))


;;; util

(defun precedence (lemmatizer pos)
  "POS tag precedence in LEMMATIZER precedence list"
  (or (position pos @lemmatizer.pos-precedence)
      most-positive-fixnum))

;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.contrib.wordnet)
(named-readtables:in-readtable rutils-readtable)


(defclass wordnet ()
  ((uri :initarg :uri :reader wordnet-uri))
  (:documentation
   "Interface to Wordnet database."))

(defclass sql-wordnet3 (wordnet)
  ((uri :initarg :uri :reader wordnet-uri
        :initform (merge-pathnames "data/wordnet30.sqlite" +project-root+)))
  (:documentation
   "Interface to Wordnet database version 3."))

(define-lazy-singleton wordnet (make 'sql-wordnet3)
  "Interface to Wordnet database version 3.")
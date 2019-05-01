;;; (c) 2013-2017 Vsevolod Dyomkin

(in-package #:nlp.contrib.wordnet)
(named-readtables:in-readtable rutils-readtable)


(defclass wordnet ()
  ((uri :initarg :uri :reader wordnet-uri))
  (:documentation
   "Interface to Wordnet database."))

(defclass sql-wordnet3 (wordnet)
  ((uri :initarg :uri :reader wordnet-uri
        :initform (data-file "wordnet30.sqlite")))
  (:documentation
   "Interface to Wordnet database version 3."))

(def-lang-var wordnet (make 'sql-wordnet3)
  "Interface to Wordnet database version 3.")

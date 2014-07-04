;;; (c) 2013-2014 Vsevolod Dyomkin

(in-package #:nlp.tagging)
(named-readtables:in-readtable rutils-readtable)


(defgeneric tag (model sentence)
  (:documentation
   "Tag a SENTENCE with the algorithm specific to MODEL."))

(defclass tagger (categorical-model)
  ())
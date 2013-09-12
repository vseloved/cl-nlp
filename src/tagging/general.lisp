;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.tagging)
(named-readtables:in-readtable rutils-readtable)


(defparameter +stop-tag+ :stop
  "Name of ending tag in POS tagging.")

(defgeneric tag (model sentence)
  (:documentation
   "Tag a SENTENCE with the algorithm specific to MODEL."))
;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.syntax)
(named-readtables:in-readtable rutils-readtable)


(defparameter +stop-tag+ :stop
  "Name of ending tag in POS tagging.")

(defgeneric pos-tag (model sentence)
  (:documentation
   "Tag a SENTENCE with the algorithm specific to MODEL."))
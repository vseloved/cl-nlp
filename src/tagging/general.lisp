;;; (c) 2013-2014 Vsevolod Dyomkin

(in-package #:nlp.tagging)
(named-readtables:in-readtable rutils-readtable)


(defgeneric tag (model sent)
  (:documentation
   "Tag a SENTence with the algorithm specific to MODEL."))

(defclass tagger (categorical-model)
  ()
  (:documentation
   "A tagger adds tags to sentence tokens."))

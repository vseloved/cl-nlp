;;; (c) 2013-2017 Vsevolod Dyomkin

(in-package #:nlp.tagging)
(named-readtables:in-readtable rutilsx-readtable)


(defgeneric tag (model sent)
  (:documentation
   "Tag a SENTence with the algorithm specific to MODEL."))

(defclass tagger ()
  ()
  (:documentation
   "A tagger adds tags to sentence tokens."))

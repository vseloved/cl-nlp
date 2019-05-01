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


(defparameter <s> (make-ent :id 0 :beg 0 :end 0 :word "<S>" :lemma "<S>"))

(defun </s> (tok)
  (make-ent :id (1+ @tok.id)
            :beg @tok.end :end @tok.end
            :word "</S>" :lemma "</S>"))

;;; (c) 2016 Vsevolod Dyomkin

(in-package #:nlp.embeddings)
(named-readtables:in-readtable rutilsx-readtable)


(defclass vecs ()
  ((order :initarg :order :initform (error "Vecs order should be supplied")
          :accessor vecs-order))
  (:documentation
   "Word vectors access point."))

(defgeneric 2vec (vecs word &key normalize)
  (:documentation
   "Return a vector representation of the WORD according to VECS.")
  (:method :around (vecs word &key normalize)
    (call-next-method vecs (if normalize
                               (normalize vecs word)
                               word))))

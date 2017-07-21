;;; (c) 2016 Vsevolod Dyomkin

(in-package #:nlp.embeddings)
(named-readtables:in-readtable rutilsx-readtable)


(defclass vecs ()
  ((order :initarg :order :initform (error "Vecs order should be supplied")
          :accessor vecs-order))
  (:documentation
   "Word vectors access point."))

(defmethod normalize ((form vecs) word)
  word)

(defgeneric 2vec (vecs word)
  (:documentation
   "Return a vector representation of the WORD according to VECS.")
  (:method :around (vecs word)
    (or (call-next-method vecs (normalize vecs word))
        (unk vecs))))
  
(defgeneric unk (vecs)
  (:documentation
   "Return a vector for unknown word according to VECS.")
  (:method (vecs)
    (make-array @vecs.order :initial-element 0.0)))


(defmethod normalize ((form vecs) word)
  (cond-it
    ((re:scan *number-regex* word) (case (length word)
                                     (1 "0")
                                     (2 "00")
                                     (3 "00h")
                                     (4 "00k")
                                     ((5 6) "0kk")
                                     (7 "00m")
                                     (otherwise "00+")))
    ((re:scan *email-regex* word) "@__")
    ((re:scan *url-regex* word) "@@@")
    ((> (length word) 30) nil)
    (t (string-downcase word))))

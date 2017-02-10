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
    (call-next-method vecs (normalize vecs word))))


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

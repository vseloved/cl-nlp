;;; (c) 2014 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutils-readtable)


(defclass greedy-ap-depparser (avg-perceptron depparser)
  ()
  (:documentation
   "A greedy averaged perceptron dependency parser."))

(defmethod parse ((parser greedy-ap-depparser) (sent sent))
  )

(defmethod extract-fs ((model greedy-ap-depparser) &rest args)
  )

(defmethod train ((model greedy-ap-depparser) sents &key (epochs 5) verbose)
  )

(defmethod extract-gold ((model greedy-ap-depparser) data)
  )

;;; (c) 2013-2016 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutils-readtable)


;;; parser

(defclass parser ()
  ()
  (:documentation
   "A parser parser sentences to some representation."))

(defclass parsed-sent (sent)
  ((tree :initarg :tree :accessor sent-tree)
   (deps :initarg :deps :accessor sent-deps)
   (amr :initarg :amr :accessor sent-amr))
  (:documentation
   "Sententce with a constituency TREE, DEPS, and AMR graph."))

(defgeneric parse (parser sent)
  (:documentation
   "Parse SENT with a PARSER.")
  (:method :around (parser (sent string))
    (call-next-method parser (tokenize <word-tokenizer> sent))))


;;; oracle

(defclass oracle ()
  ()
  (:documentation
   "Oracle for parser training."))

(defclass ranking-oracle (oracle)
  ((limiter :initarg limiter :accessor oracle-limiter
            :initform (warn "Limiter not defined for ranking-amroracle")))
  (:documentation
   "Deterministic oracle that allows only a limited number
    of actions (that are sorted according to their uitlity)."))

(defclass top-amroracle (ranking-oracle)
  ((limiter :initarg limiter :accessor oracle-limiter
            :initform (constantly 1)))
  (:documentation
   "Ranking oracle that allows the top-ranked transition."))

(defclass better-oracle (ranking-oracle)
  ((limiter :initarg limiter :accessor oracle-limiter
            :initform #`(position-if (complement #'plusp) % :key #'rt)))
  (:documentation
   "Ranking oracle that allows only transitions
    that transform the graph to the better."))

(defclass best-oracle (ranking-oracle)
  ((limiter :initarg limiter :accessor oracle-limiter
            :initform (lambda (transitions)
                        (let ((best (rt (first transitions))))
                          (position-if #`(< % best) transitions :key #'rt)))))
  (:documentation
   "Ranking oracle that allows only transitions
    that transform the intermediate result in the best way."))

;;; (c) 2015 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutils-readtable)


;;; parser

(defclass conparser ()
  ()
  (:documentation
   "Constituency parser that returns a parse tree of tokens from PARSE."))


;; Parser mixins

(defclass pretagged ()
  ()
  (:documentation
   "A mixin for pretagged grammars."))

(defclass lexicalized ()
  ()
  (:documentation
   "A mixin for lexicalized grammars."))

(defclass markovized ()
  ((order :initarg order :initform 1 :reader markov-order))
  (:documentation
   "A mixin for markovized grammars."))

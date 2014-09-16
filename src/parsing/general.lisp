;;; (c) 2013-2014 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutils-readtable)


(defgeneric parse (parser sentence)
  (:documentation
   "Parse SENTENCE with a PARSER.")
  (:method :around (parser (sentence string))
    (call-next-method parser (tokenize <word-tokenizer> sentence))))


;; Parser types

(defclass conparser ()
  ()
  (:documentation
   "Constituency parser that returns a parse tree of tokens from PARSE."))

(defclass depparser ()
  ()
  (:documentation
   "Dependency parser that returns a list of dependencies from PARSE."))

(defstruct (dep (:conc-name "dep-")
                (:print-object (lambda (dep stream)
                                 "Print DEP in Stanford standard dependency format."
                                 (with-slots (rel govr dept) dep
                                   (format stream "~@[~(~A~)~](~A, ~A)"
                                           rel govr dept)))))
  (rel nil)
  govr
  dept)


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

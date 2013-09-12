;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutils-readtable)


(defgeneric parse (parser model sentence)
  (:documentation
   "Parse SENTENCE with a PARSER and MODEL.")
  (:method :around (parser model (sentence string))
   (call-next-method parser model (tokenize <word-tokenizer> string))))

(defgeneric parse-n (parser model sentence n)
  (:documentation
   "Return N best parse trees of the SENTENCE with a PARSER and MODEL.")
  (:method :around (parser model (sentence string) n)
   (call-next-method parser model (tokenize <word-tokenizer> string) n)))


;; Grammar mixins

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

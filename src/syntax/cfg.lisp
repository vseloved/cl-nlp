;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.syntax)
(named-readtables:in-readtable rutils-readtable)


(defclass cfg ()
  ((terminals :initarg :terminals :accessor gr-ts)
   (nonterminals :initarg :nonterminals :accessor gr-nts)
   (root :initarg :root :accessor gr-root)
   (rules :initarg :rules :accessor gr-rules))
  (:documentation
   "Context-free grammar is a tuple of <TERMINALS, NONTERMINALS, ROOT, RULES>."))

(defmethod parse ((model cfg) (sentence list))
  )
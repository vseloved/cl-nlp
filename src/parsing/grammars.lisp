;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutils-readtable)


(defclass cfg ()
  ((terminals :initarg :terminals :accessor grammar-ts)
   (nonterminals :initarg :nonterminals :accessor grammar-nts)
   (root :initarg :root :accessor grammar-root)
   (unary-rules :initarg :unary-rules :accessor grammar-unary-rules)
   (binary-rules :initarg :binary-rules :accessor grammar-binary-rules))
  (:documentation
   "Context-free grammar is a tuple of <TERMINALS, NONTERMINALS, ROOT, RULES>."))

(defclass pcfg (cfg)
  ((root-rules :initarg :root-rules :reader grammar-root-rules
    :documentation
    "A special set of dyadic rules of the form ((ROOT NT) . Q)
     which specify the probability of a sentence being one of NTS
     (like S or SBAR). It may be used, if we want to have fake root node.")
   (iurules :reader grammar-iurules
    :documentation
    "A special slot for optimization purposes, which holds unary rules with
     numbers instead of non-terminals. Non-terminals are indexed in NTS-IDX.")
   (ibrules :reader grammar-ibrules
    :documentation
    "A special slot for optimization purposes, which holds binary rules with
     numbers instead of non-terminals. Non-terminals are indexed in NTS-IDX.")
   (nts-idx :reader grammar-nts-idx
    :documentation
    "A special slot for optimization purposes, which holds a bidirectional
     mapping between NTS and their indices.
     Its car maps from NTS to numbers, and cdr in the opposite direction."))
  (:documentation
   "Probabilistic context-free grammar adds QS rule probability paramters
    to an ordinary CFG."))

(defmethod slot-unbound (class (obj pcfg) (slot (eql 'nts-idx)))
  (let ((nts-idx (cons (make-hash-table) (make-hash-table))))
    (doindex (i nt (grammar-nts obj))
      (set# nt (car nts-idx) i)
      (set# i  (cdr nts-idx) nt))
    (setf (slot-value obj 'nts-idx) nts-idx)))

(defmethod slot-unbound (class (obj pcfg) (slot (eql 'iurules)))
  (let ((nts-idx (car (grammar-nts-idx obj)))
        (iurules (make-hash-table :test 'equal)))
    (dotable (rule q (grammar-unary-rules obj))
      (set# (mapcar #`(if (symbolp %) (get# % nts-idx) %)
                    rule)
            iurules q))
    (setf (slot-value obj 'iurules) iurules)))

(defmethod slot-unbound (class (obj pcfg) (slot (eql 'ibrules)))
  (let ((nts-idx (car (grammar-nts-idx obj)))
        (ibrules (make-hash-table :test 'equal)))
    (dotable (rule q (grammar-binary-rules obj))
      (set# (mapcar #`(if (symbolp %) (get# % nts-idx) %)
                    rule)
            ibrules q))
    (setf (slot-value obj 'ibrules) ibrules)))

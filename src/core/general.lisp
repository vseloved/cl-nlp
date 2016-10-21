;;; (c) 2013-2016 Vsevolod Dyomkin

(in-package #:nlp.core)
(named-readtables:in-readtable rutils-readtable)


(defstruct (token (:print-object
                   (lambda (token stream)
                     (with-slots (id word pos beg end) token
                       (format stream "<~A~@[/~A~]~@[:~A~]~@[ ~A~]>"
                               word pos id
                               (when beg
                                 (if end
                                     (fmt "~A..~A" beg end)
                                     beg)))))))
  "A corpus token with id or postition and possibly POS tag.
   Also may contain word lemma."
  id
  beg
  end
  word
  lemma
  pos)

(defmethod s! ((obj token))
  (ncore:token-word obj))

(defclass sent ()
  ((tokens :initarg :tokens :accessor sent-tokens))
  (:documentation "Basically, a sentence is a list of tokens."))

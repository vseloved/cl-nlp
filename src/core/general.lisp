;;; (c) 2013-2017 Vsevolod Dyomkin

(in-package #:nlp.core)
(named-readtables:in-readtable rutilsx-readtable)


(defstruct (tok (:print-object
                 (lambda (tok stream)
                   (format stream "<~A~@[/~A~]~@[:~A~]~@[ ~A~]>"
                           @tok.word @tok.pos @tok.id
                           (when @tok.beg
                             (if @tok.end
                                 (fmt "~A..~A" @tok.beg @tok.end)
                                 @tok.beg))))))
  "A corpus token with id or postition and possibly POS tag.
   Also may contain word lemma."
  id
  beg
  end
  word
  lemma
  pos)

(defmethod ss ((obj tok))
  @obj.word)

(defclass sent ()
  ((toks :initarg :toks :accessor sent-toks))
  (:documentation "Basically, a sentence is a list of tokens."))

(defmethod print-object ((obj sent) out)
  (print-unreadable-object (obj out :identity t)
    (format out "SENT: ~{~A~^ ~}" @obj.toks)))

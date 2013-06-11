;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.syntax)
(named-readtables:in-readtable rutils-readtable)


(defgeneric parse (model sentence)
  (:documentation
   "Parse SENTENCE with MODEL.")
  (:method :around (model (sentence string))
   (call-next-method model (tokenize <word-tokenizer> string))))

(defgeneric parse-n (model sentence n)
  (:documentation
   "Return N best parse trees of the SENTENCE with MODEL.")
  (:method :around (model (sentence string) n)
   (call-next-method model (tokenize <word-tokenizer> string) n)))

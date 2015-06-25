;;; (c) 2015 Vsevolod Dyomkin

(in-package #:nlp.lexics)
(named-readtables:in-readtable rutils-readtable)


(defgeneric stemmize (stemmer word)
  (:documentation
   "Stemmize WORD string with a STEMMER."))

(defgeneric lemmatize (lemmatizer word &optional tag)
  (:documentation
   "Lemmatize WORD string with a LEMMATIZER."))

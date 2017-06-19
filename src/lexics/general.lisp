;;; (c) 2015-2017 Vsevolod Dyomkin

(in-package #:nlp.lexics)
(named-readtables:in-readtable rutilsx-readtable)


;;; Basic classes

(defclass dict ()
  ()
  (:documentation
   "A dictionary."))

(defclass stemmer ()
  ()
  (:documentation
   "A generic stemmer."))

(defclass lemmatizer ()
  ()
  (:documentation
   "A generic lemmatizer."))


;;; Core API

(defgeneric lookup (dict word)
  (:documentation
   "Lookup WORD in DICT.")
  (:method ((dict hash-table) word)
    (get# word dict)))

(defgeneric pos-tags (dict word)
  (:documentation
   "Get WORD pos tags from DICT.")
  (:method (dict word)
    (error 'not-implemented-error)))

(defgeneric stem (stemmer word)
  (:documentation
   "Stemmize WORD string with a STEMMER."))

(defgeneric lemmatize (lemmatizer word &optional pos)
  (:documentation
   "Lemmatize WORD (with optionally specified POS tag) string with a LEMMATIZER."))

(defgeneric guess-lemma (lemmatizer word &optional pos)
  (:documentation
   "Guess the most appropriate lemma for the WORD
    (with optionally specified POS tag) accoding to LEMMATIZER."))

(defun morph (lemmatizer word pos)
  "Change WORD form to a desired POS tag using LEMMATIZER."
  (when (lookup @lemmatizer.dict word)
    (if (member pos (pos-tags word))
        word
        (lemmatize lemmatizer (lemmatize lemmatizer word) pos))))


;;; utilities

(defun word/pos (word &optional pos)
  "Stringify WORD with its POS tag."
  (fmt "~A~@[/~{~A~^:~}~]" word (mklist pos)))

(defun base-pos (pos)
  "Return the base pos tag from a POS (for instance, VBP turns to VB)."
  (when pos
    (let ((postr (symbol-name pos)))
      (mksym (slice postr 0 (min 2 (length postr)))
             :package (symbol-package pos)))))

;;; (c) 2015-2019 Vsevolod Dyomkin

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
   "Lemmatize WORD (with optionally specified POS tag) string with a LEMMATIZER.
    Should return as first value the most appropriate lemma or NIL if no exists,
    as a second value - the lemma's POS tag or NIL (if lemma is NIL),
    and as a third value - a list of other possible lemma-tag pairs if any."))

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

(defun pos-tag (word)
  "Return the first pos tag of the WORD."
  (atomize (first (nlex:pos-tags nlex:*dict-lemmatizer* word))))

(defun word-shape (word)
  "Return the shape of the word as a string of characters:
   - u for all upper-case groups
   - l for all lower-case groups
   - d for all digit groups
   - - for all other character groups

   Example: foo -> l, Foo -> ul, FOO -> u, foo:bar123 -> l-ld"
  (coerce (loop :with prev
                :for char :across word
                :for cur := (cond ((upper-case-p char) #\u)
                                  ((lower-case-p char) #\l)
                                  ((digit-char-p char) #\d)
                                  (t #\-))
                :unless (eql cur prev)
                  :collect cur :and :do (:= prev cur))
          'string))


;;; stopwords

(def-lang-var stopwords #h() "Stopwords")

(defun stopwordp (word)
  (in# word <stopwords>))

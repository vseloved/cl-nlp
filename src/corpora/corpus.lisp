;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defstruct corpus
  "A corpus is a NAMEd collection of TEXTS that may be groupped into GROUPS."
  name
  texts
  groups)

(defstruct text
  "A single text from a certain corpus with some logical NAME.
   A text is stored forms: RAW, CLEAN and tokenized (TOKENS)."
  name
  raw
  clean
  tokens)


(defgeneric read-corpus (type path)
  (:documentation
   "Read the whole corpus of a certain TYPE (a keyword) from some PATH."))

(defgeneric read-corpus-file (type file)
  (:documentation
   "Read corpus data of a certain TYPE from FILE.
    Returns as values:
    - raw text
    - clean text
    - list of tokens from the text
    - possibly some other relevant data"))

(defparameter *corpora-root* (merge-pathnames "corpora/*" +project-root+)
  "Default root directory for corpuses.")
;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defstruct corpus
  "A corpus is a collection of TEXTS that may be groupped into GROUPS."
  desc
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

(defgeneric map-corpus (type path fn)
  (:documentation
   "Map FN to each entry (of type TEXT) in corpus of TYPE (a keyword) at PATH.
    The order of processing the corpus' entries is unspecified.
    Similar to MAPHASH rather than MAPCAR:
    result collection, if needed, should be done in the FN."))


(defparameter *corpora-root* (merge-pathnames "corpora/*" +project-root+)
  "Default root directory for corpuses.")

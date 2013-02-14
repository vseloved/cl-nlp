;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defstruct corpus
  "A structure to cache a unilingual corpus in raw and processed forms."
  name
  lang
  raw-texts
  clean-texts
  text-tokens)

(defstruct token
  "A corpus token with postition and possibly tag."
  beg
  end
  word
  tag)


(defgeneric read-corpus (type file)
  (:documentation
   "Read corpus data of a certain TYPE (a keyword) from file.
    Returns as values:
    - raw text data
    - cleaned-up text
    - list of tokens from the text"))
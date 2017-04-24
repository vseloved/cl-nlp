;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defstruct corpus
  "A corpus is a collection of TEXTS that may be groupped into GROUPS."
  desc
  texts
  groups)

(defstruct (text (:print-object print-text))
  "A single text from a certain corpus with some logical NAME.
   A text is stored in the following forms:
   - RAW - unprocessed, as it is in the corpus
           (if possible: for instance, for XML corpora ir doesn't make sense)
   - CLEAN - processed, but in plain format (just words)
   - PARAG-SENT-TOKS - paragraph-sentence-token 3-level structure"
  name
  raw
  clean
  parag-sent-toks)

(defun print-text (text stream)
  (with-slots (name raw clean parag-sent-toks) text
    (format stream "#S(TEXT :NAME \"~A\"~
                       ~@[~%:RAW \"~A ...\"~]~
                       ~@[~%:CLEAN \"~A ...\"~]~
                       ~@[~%:PARAG-SENT-TOKS (~{~A ~}...)~])"
            name
            (when raw (sub raw 0 (min (length raw) 100)))
            (when clean (sub clean 0 (min (length raw) 100)))
            (when parag-sent-toks
              (subseq parag-sent-toks 0 (min (length parag-sent-toks) 10))))))


(defgeneric read-corpus-file (type file &key &allow-other-keys)
  (:documentation
   "Read corpus data of a certain TYPE from FILE.
    Returns as values:
    - raw text
    - clean text
    - list of paragraphs-sentences-tokens from the text
    - possibly some other relevant data"))

(defgeneric read-corpus (type path &key ext &allow-other-keys)
  (:documentation
   "Read the whole corpus of a certain TYPE (a keyword) from some PATH.
    If EXT is given it is a restriction on file's extension."))

(defgeneric map-corpus (type path fn &key ext &allow-other-keys)
  (:documentation
   "Map FN to each entry (of type TEXT) in corpus of TYPE (a keyword) at PATH.
    The order of processing the corpus' entries is unspecified.
    Similar to MAPHASH rather than MAPCAR:
    result collection, if needed, should be done in the FN.
    If EXT is given it is a restriction on file's extension."))

;;; (c) 2013-2017 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutilsx-readtable)


(defun make-corpus-from-dir (name dir &key ext (test 'identity))
  "Make a corpus named NAME of texts from files in DIR
   (optionally limited by extension EXT)."
  (let ((corpus (make-corpus :name name)))
    (dofiles (file dir :ext ext)
      (when (call test (pathname-name %))
        (let ((raw (read-file %)))
          (push (make-text :name (pathname-name %)
                           :raw raw
                           :parag-sent-toks (tokenize <full-text-tokenizer> raw))
                @corpus.texts))))
    (reversef @corpus.texts)
    corpus))


;;; pre-defined corpora

(def-lang-var brown-corpus
  (let ((dir (corpus-file "brown/")))
    (format *debug-io* "~&Reading Brown corpus from: ~A - " dir)
    (read-corpus :brown dir)
    (format *debug-io* "done.~%"))
  "Brown University Standard Corpus of Present-Day American English.")

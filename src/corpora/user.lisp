;;; (c) 2013-2014 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defun make-corpus-from-dir (name dir &key (test 'identity))
  "Make a corpus named NAME of texts from files in DIR.
   Optionally, filenames may need to satisfy TEST."
  (let (texts)
    (fad:walk-directory dir
                        #`(when (funcall test (pathname-name %))
                            (push (pair (pathname-name %)
                                        (read-file %))
                                  texts)))
    (setf texts (mapcar #`(make-text
                           :name (lt %)
                           :raw (rt %)
                           :tokens
                           (mapcar #`(ncore:make-token :word %)
                                   (mapcan #`(ncore:tokenize
                                              ncore:<word-tokenizer> %)
                                           (ncore:tokenize
                                            ncore:<sentence-splitter> (rt %)))))
                        texts))
    (make-corpus :desc name
                 :texts texts)))


;;; pre-defined corpora

(defparameter +brown-corpus+
  (read-corpus :brown (corpus-file "brown/"))
  "Brown University Standard Corpus of Present-Day American English.")

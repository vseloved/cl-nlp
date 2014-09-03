;;; (c) 2013-2014 Vsevolod Dyomkin

(in-package #:nlp.tags)
(named-readtables:in-readtable rutils-readtable)


(defun export-tag (str)
  "Intern and export STR in package, then return it."
  (let ((tag (intern str)))
    (export tag)
    tag))

(defparameter *word-tags* (dict-from-file (src-file "syntax/word-tags.txt")
                                          :key-transform #'export-tag)
  "Word-level tags.")

(defparameter *phrase-tags* (dict-from-file (src-file "syntax/phrase-tags.txt")
                                            :key-transform #'export-tag)
  "Phrase-level tags.")
